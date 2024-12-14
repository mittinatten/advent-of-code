import assert from "node:assert";
import fs from "node:fs";
import path from "node:path";
import { uniq } from "lodash-es";

const sampleInput = `....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...`;

const puzzleInput = fs.readFileSync(path.join(import.meta.dirname, "input.txt")).toString();

function parseInput(input) {
  return input.split("\n").map(row => row.trim()).filter(Boolean).map(row => [...row]);
}

function findGuard(map) {
  const guardChars = ["^", "<", ">", "v"];
  for (let i = 0; i < map.length; ++i) {
    for (let j = 0; j < map[i].length; ++j) {
      if (guardChars.includes(map[i][j])) return [i, j];
    }
  }
}

function displayMap(map) {
  console.log(map.map(row => row.join("")).join("\n") + "\n");
}

function nextPosition(guardOrientation, [i, j]) {
  switch (guardOrientation) {
    case "^": return [i - 1, j];
    case ">": return [i, j + 1];
    case "v": return [i + 1, j];
    case "<": return [i, j - 1];
    default:
      throw Error("Unknown guard orientation: " + guardOrientation);
  }
}

function rotate(guardOrientation) {
  switch (guardOrientation) {
    case "^": return ">";
    case ">": return "v";
    case "v": return "<";
    case "<": return "^";
    default:
      throw Error("Unknown guard orientation: " + guardOrientation);
  }
}

function updateMap(map, [i, j], value) {
  map[i][j] = value;
}

// solving this recursively is too deep for Node
function getTrace(map, initialPosition) {
  const trace = [initialPosition.join(",")];
  let currentPos = initialPosition;
  let seenConfigs = new Set();

  while (true) {
    const [i, j] = currentPos;
    const guard = map[i][j];
    const config = `${guard}-${i} -${j}`;
    if (seenConfigs.has(config)) {
      return [true, trace];
    }
    seenConfigs.add(config);

    const nextPos = nextPosition(guard, currentPos);
    const [i_next, j_next] = nextPos;

    if (i_next < 0 || i_next >= map.length || j_next < 0 || j_next >= map[0].length) {
      return [false, trace]
    }

    const nextTile = map[i_next][j_next];

    if (nextTile === ".") {
      updateMap(map, currentPos, ".");
      updateMap(map, nextPos, guard);
      trace.push(nextPos.join(','));
      currentPos = nextPos;
    }

    if (nextTile === "#") {
      updateMap(map, currentPos, rotate(guard));
    }
  }
}

function part1(input) {
  const map = parseInput(input);
  const initialPosition = findGuard(map);
  const [_, trace] = getTrace(map, initialPosition);
  return uniq(trace).length;
}

assert(part1(sampleInput) === 41);
console.log("Part 1:", part1(puzzleInput));

function cloneMapWithValue(map, [i, j], value) {
  const newMap = [...map].map(row => [...row]);
  newMap[i][j] = value;
  return newMap;
}

// Brute force
function part2(input) {
  const map = parseInput(input);
  const initialPosition = findGuard(map);

  let count = 0;
  for (let i = 0; i < map.length; ++i) {
    for (let j = 0; j < map[0].length; ++j) {
      if (map[i][j] === ".") {
        const obstructedMap = cloneMapWithValue(map, [i, j], '#');
        const [isLoop, _] = getTrace(obstructedMap, initialPosition);
        if (isLoop) {
          count++;
        }
      }
    }
  }

  return count;
}

assert(part2(sampleInput) === 6);
console.log("Part 2:", part2(puzzleInput));
