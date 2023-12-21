(import test)
(import srfi-1)
(import list-utils)

(define (get-instruction input address) (drop input address))
(define (get-opcode input address) (first (get-instruction input address)))
(define (get-operands input address) (let ((data (get-instruction input address)))
					(list (first (drop input (second data)))
					      (first (drop input (third data))))))
(define (get-op-target input address) (fourth (get-instruction input address)))
(define (calc-result operator operands) (operator (car operands) (cdar operands)))
(define (replace-nth list index value)
  (if (= index 0) (cons value (cdr list))
      (cons (car list) (replace-nth (cdr list) (- index 1) value))))

(define (runIntCode input address)
  (let ((opcode (get-opcode input address)))
	(if (equal? opcode 99)
	    input
	    (let* ((operands (get-operands input address))
		   (target (get-op-target input address))
	           (result (case opcode 
			     ((1) (replace-nth input target (fold + 0 operands)))
			     ((2) (replace-nth input target (fold * 1 operands))))))
		   (runIntCode result (+ address 4))))))

(test '(2 0 0 0 99) (runIntCode '(1 0 0 0 99) 0))
(test '(2 3 0 6 99) (runIntCode '(2 3 0 3 99) 0))
(test '(2 4 4 5 99 9801) (runIntCode '(2 4 4 5 99 0) 0))
(test '(30 1 1 4 2 5 6 0 99) (runIntCode '(1 1 1 4 99 5 6 0 99) 0))

(define input 	'(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 9 1 19
	    1 5 19 23 2 9 23 27 1 27 5 31 2 31 13 35
	    1 35 9 39 1 39 10 43 2 43 9 47 1 47 5 51
	    2 13 51 55 1 9 55 59 1 5 59 63 2 6 63 67
	    1 5 67 71 1 6 71 75 2 9 75 79 1 79 13 83
	    1 83 13 87 1 87 5 91 1 6 91 95 2 95 13 99
	    2 13 99 103 1 5 103 107 1 107 10 111 1 111
	    13 115 1 10 115 119 1 9 119 123 2 6 123 127
	    1 5 127 131 2 6 131 135 1 135 2 139 1 139 9
	    0 99 2 14 0 0))

(define (replace-two list key-value-1 key-value-2)
  (replace-nth
   (replace-nth list (car key-value-1) (cadr key-value-1))
   (car key-value-2) (cadr key-value-2)))
  
(define input-2-1 (replace-two input '(1 12) '(2 2)))

(print "2-1: " (car (runIntCode input-2-1 0)))



(define (search-operands input target max initial-value)
  (define (search-second-operand first second)
    (if (> second max) 'not-found
        (let* ((new-input (replace-two input (list 1 first) (list 2 second)))
               (result (car (runIntCode new-input 0))))
          (cond ((equal? target result) (list first second))
                ((= (+ 1 second) (length input)) 'not-found)
                (else (search-second-operand first (+ second 1)))))))
                                     
  (define (search-first-operand first second)
    (print first second)
    (if (> first max)
        'not-found
        (let*  ((new-input (replace-two input (list 1 first) (list 2 second)))
          (result (car (runIntCode new-input 0))))
          (if (equal? target result) (list first second)
              (let ((operands (search-second-operand first second)))
                (if (equal? operands 'not-found)
                    (search-first-operand (+ first 1) second)
                    operands))))))

  (search-first-operand (car initial-value) (cadr initial-value)))


(test '(0 0) (search-operands '(1 0 0 0 99) 2 99 '(0 0)))
(test '(2 2) (search-operands '(1 0 2 0 99) 4 99 '(0 0)))

(print "2-2: " (search-operands input 19690720 99 '(0 0)))

