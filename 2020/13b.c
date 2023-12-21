// C99, uses Posix threads
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

#define MAX_INPUT_LENGTH 200

typedef struct bus {
  int offset;
  int number;
} bus;

typedef struct schedule {
  bus *buses;
  int n;
} schedule;

int compare_bus(const void *bus1, const void *bus2) {
  int n1 = ((bus*) bus1)->number;
  int n2 = ((bus*) bus2)->number;

  return n1 > n2 ? -1 : 1;
}

schedule parse(char *input) {
  bus *buses = malloc(sizeof(bus) * 200);
  char *bus = strtok(input,",");
  int i, n;
  schedule s = { buses, 0 };
  
  printf("Schedule: ");
  for (i = 0; bus; ++i) {
    if (bus[0] != 'x') {
      int number = atoi(bus);
      buses[s.n].offset = i;
      buses[s.n].number = number;
      printf("%d %d, ", i, number);
      ++s.n;
    }
    bus = strtok(NULL,",");
  }
  printf("\n");

  // largest first, no point in checking smaller values
  qsort(buses, s.n, sizeof(bus), &compare_bus);

  return s;
}

int is_match(long time, schedule s) {
  int i = 0;
  bus *b = s.buses;

  while (i < s.n && (time + b[i].offset) % b[i].number == 0) {
    ++i;
  }

  return i == s.n;
}

long find_time(schedule s, long i0, long nsteps) {
  int step = s.buses[0].number;
  long first = -s.buses[0].offset + step*i0, last = i0 + nsteps;

  for (long t = first, i = i0; i < last; t += step, ++i) {
    if (is_match(t, s)) {
      return t;
    }
  }

  return 0;
}



typedef struct thread_data {
  long start;
  long n_steps;
  schedule s;
} thread_data;

void * run_thread(void *arg) {
  thread_data *data = (thread_data*) arg;
  long *found_it = malloc(sizeof(long));
  *found_it = find_time(data->s, data->start, data->n_steps);
 
  pthread_exit(found_it);
}

void run(schedule s, int n_threads) {
  pthread_t threads[n_threads];
  thread_data data[n_threads];
  long start = 0, steps_per_thread = 1000000000; 
  int done = 0;

  printf("Calculating using %d threads.\n", n_threads);
  
  while (done == 0) {
    for (int i = 0; i < n_threads; ++i) {
      data[i].start = start;
      data[i].n_steps = steps_per_thread;
      data[i].s = s;
      pthread_create(&threads[i], NULL, run_thread, &data[i]);
      printf("Checking interval [%ld,%ld) in thread %d.\n", start, start + steps_per_thread, i);
      start += steps_per_thread;
    }
    for (int i = 0; i < n_threads; ++i) {
      long *result;
      pthread_join(threads[i], (void *) &result);
      if (*result > 0) {
        done = 1;
        printf(">>>> Answer: %ld <<<<\n", *result);

        for (int j = i + 1; j < n_threads; ++j) {
          pthread_cancel(threads[i]);
        }
        break;
      }
      free(result);
    }
  }
}

int main(int argc, char **argv) {
  int n_threads = 6;
  char input[MAX_INPUT_LENGTH];

  scanf("%s", input);

  schedule s = parse(input);

  if (argc > 1) {
    n_threads = atoi(argv[1]);
  }
  
  run(s, n_threads);
  
  return 0;
}
