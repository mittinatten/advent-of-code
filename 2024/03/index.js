import fs from "node:fs";
import path from "node:path";
import assert from "node:assert";
import { sum } from 'lodash-es';

function sumProducts(exp) {
  const terms = exp
    .match(/mul\([\d]{1,3},[\d]{1,3}\)/g)
    .map(e => /mul\((?<x>\d+),(?<y>\d+)\)/.exec(e).groups);
  return sum(terms.map(f => f.x * f.y));
}

const sampleInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5)))"
const puzzleInput = fs.readFileSync(path.join(import.meta.dirname, "input.txt")).toString()

assert(sumProducts(sampleInput) === 161);
console.log("Part 1:", sumProducts(puzzleInput))

function filterOutDisabledExpressions(input) {
  return input.split("\n").join("").replaceAll(/(don't\(\)).*?(do\(\)|$)/g, "")
}

function sumEnabledExpressions(input) {
  const exp = filterOutDisabledExpressions(input);
  return sumProducts(exp);
}

assert(sumEnabledExpressions("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))") === 48)
assert(sumEnabledExpressions("xmul(2,4)&mul[3,7]!^don't()_do()mul(5,5)+mul(32,64]don't()(mul(11,8)undo()?mul(8,5))") === 48 + 25)
console.log("Part 2:", sumEnabledExpressions(puzzleInput))
