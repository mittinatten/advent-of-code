import { sum, sortBy } from "lodash-es"
import assert from "node:assert";
import fs from "node:fs";
import path from "node:path";

const sampleInput = `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`

function parseInput(input) {
  const rows = input.split("\n");
  const splitIndex = rows.findIndex(r => r === "");

  const rules = rows.slice(0, splitIndex)
    .map(rule => rule.split("|"))
    .reduce((acc, [before, after]) =>
      ({ ...acc, [before]: [...(acc[before] ?? []), after] }), {});

  const updates = rows.slice(splitIndex + 1).filter(Boolean).map(update => update.split(","));

  return { rules, updates };
}

function isUpdateValid(rules, update) {
  for (let i = update.length - 1; i >= 0; i--) {
    const pagesBefore = update.slice(0, i);
    if (rules[update[i]]?.some(mustBeAfter => pagesBefore.includes(mustBeAfter))) {
      return false;
    }
  }
  return true;
}

function part1(data) {
  const validUpdates = data.updates.map(update => isUpdateValid(data.rules, update) ? update : undefined).filter(Boolean);
  const middles = validUpdates.map(update => update[Math.floor(update.length / 2)]).map(v => Number.parseInt(v));
  return sum(middles);
}

const sample = parseInput(sampleInput);
assert(part1(sample) === 143);

const puzzleInput = fs.readFileSync(path.join(import.meta.dirname, "input.txt")).toString();
const puzzleData = parseInput(puzzleInput);
console.log("Part 1:", part1(puzzleData));

function fixInvalidUpdate(rules, update) {
  return update.toSorted((a, b) => rules[a]?.includes(b) ? -1 : 1)
}

function part2(data) {
  const fixed = data.updates
    .filter(update => !isUpdateValid(data.rules, update))
    .map(update => fixInvalidUpdate(data.rules, update));
  const middles = fixed.map(update => update[Math.floor(update.length / 2)]).map(v => Number.parseInt(v));
  return sum(middles);
}

assert(part2(sample) === 123);
console.log("Part 2:", part2(puzzleData));
