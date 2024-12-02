import fs from "node:fs";
import path from "node:path";
import assert from "node:assert";

function readLevels(fileName) {
  return fs
    .readFileSync(path.join(import.meta.dirname, fileName))
    .toString()
    .split("\n")
    .filter(Boolean)
    .map(line => line.split(/\s+/));
}

function isSafe(level, threshold = 1) {
  let direction = 0;
  for (let i = 1; i <= level.length - 1; ++i) {
    const d = level[i] - level[i - 1];

    if (d === 0) {
      return false;
    }

    if (direction === 0) {
      direction = d;
    } else if ((d < 0 && direction > 0) || (d > 0 && direction < 0)) {
      return false;
    }

    if (Math.abs(d) > 3) {
      return false;
    }
  }
  return true
}

assert(isSafe([]));
assert(isSafe([1]));
assert(isSafe([1, 2]));
assert(isSafe([1, 2, 3]));
assert(isSafe([1, 1, 3]) === false);
assert(isSafe([3, 1, 1]) === false);

function countSafeLevels(levels, predicate = isSafe) {
  return levels.filter(predicate).length
}

const sampleLevels = readLevels("./sample.txt");
assert(countSafeLevels(sampleLevels) == 2);

const levels = readLevels("./input.txt");
console.log("Part 1", countSafeLevels(levels));


function isSafe2(level) {
  if (isSafe(level)) return level;
  const copies = level.map((_, i) => level.filter((_, j) => j != i));
  return copies.some(isSafe);
}

assert(isSafe2([1, 2, 3]))
assert(isSafe2([1, 2, 2, 3]))
assert(isSafe2([1, 2, 2, 2, 3]) === false)
assert(countSafeLevels(sampleLevels, isSafe2) === 4)

console.log("Part 2", countSafeLevels(levels, isSafe2))
