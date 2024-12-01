import fs from "node:fs";
import path from "node:path";
import assert from "node:assert";
import { zipWith, sum, groupBy } from "lodash-es";

function readLists(fileName) {
  const [list1, list2] = fs
    .readFileSync(path.join(import.meta.dirname, fileName))
    .toString()
    .split("\n")
    .filter(Boolean)
    .map(line => line.split(/\s+/))
    .reduce(([list1, list2], [v1, v2]) => ([list1.concat([v1]), list2.concat([v2])]), [[], []]);
  return [list1.sort(), list2.sort()]
}

function part1(list1, list2) {
  return sum(zipWith(list1, list2, (a, b) => Math.abs(a - b)));
}

function part2(list1, list2) {
  const groups = groupBy(list2);
  return sum(list1.map(v => (groups[v]?.length || 0) * v));
}

const [sample1, sample2] = readLists("./sample1.txt")
const sampleResult1 = part1(sample1, sample2);
assert(sampleResult1 == 11, "Correct sample result part 1")

const [list1, list2] = readLists("./input1.txt")

console.log("Part 1:", part1(list1, list2))

const sampleResult2 = part2(sample1, sample2);
assert(sampleResult2 == 31);

console.log("Part 2:", part2(list1, list2))
