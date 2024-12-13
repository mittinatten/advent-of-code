import { sum, reverse } from "lodash-es"
import assert from "node:assert";
import fs from "node:fs";
import path from "node:path";

const sample1 = `MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX`;

function parseInput(input) {
  return input.split("\n").map(row => row.trim()).filter(Boolean);
}

function toString(array) {
  return array.filter(Boolean).join("");
}

const east = (table, i, j) => [table[i][j], table[i][j + 1], table[i][j + 2], table[i][j + 3]];
const west = (table, i, j) => [table[i][j], table[i][j - 1], table[i][j - 2], table[i][j - 3]];
const north = (table, i, j) => [table[i][j], table[i - 1]?.[j], table[i - 2]?.[j], table[i - 3]?.[j]];
const south = (table, i, j) => [table[i][j], table[i + 1]?.[j], table[i + 2]?.[j], table[i + 3]?.[j]];
const northEast = (table, i, j) => [table[i][j], table[i + 1]?.[j + 1], table[i + 2]?.[j + 2], table[i + 3]?.[j + 3]];
const northWest = (table, i, j) => [table[i][j], table[i + 1]?.[j - 1], table[i + 2]?.[j - 2], table[i + 3]?.[j - 3]];
const southWest = (table, i, j) => [table[i][j], table[i - 1]?.[j - 1], table[i - 2]?.[j - 2], table[i - 3]?.[j - 3]];
const southEast = (table, i, j) => [table[i][j], table[i - 1]?.[j + 1], table[i - 2]?.[j + 2], table[i - 3]?.[j + 3]];

function countXmas(table) {
  let count = 0
  for (let i = 0; i < table.length; ++i) {
    for (let j = 0; j < table[i].length; ++j) {
      if (table[i][j] !== 'X') continue;
      count += [
        toString(east(table, i, j)) === "XMAS",
        toString(west(table, i, j)) === "XMAS",
        toString(north(table, i, j)) === "XMAS",
        toString(south(table, i, j)) === "XMAS",
        toString(northEast(table, i, j)) === "XMAS",
        toString(northWest(table, i, j)) === "XMAS",
        toString(southWest(table, i, j)) === "XMAS",
        toString(southEast(table, i, j)) === "XMAS",
      ].filter(Boolean).length;
    }
  }
  return count
}

const sampleTable = parseInput(sample1);
assert(countXmas(sampleTable) == 18);

const puzzleInput = fs.readFileSync(path.join(import.meta.dirname, "input.txt")).toString()
console.log("Part 1: ", countXmas(parseInput(puzzleInput)))


const diagonal1 = (table, i, j) => [table[i - 1]?.[j - 1], table[i][j], table[i + 1]?.[j + 1]];
const diagonal2 = (table, i, j) => [table[i - 1]?.[j + 1], table[i][j], table[i + 1]?.[j - 1]];
const diagonal3 = (table, i, j) => [table[i + 1]?.[j + 1], table[i][j], table[i - 1]?.[j - 1]];
const diagonal4 = (table, i, j) => [table[i + 1]?.[j - 1], table[i][j], table[i - 1]?.[j + 1]];

function countMasX(table) {
  let count = 0
  for (let i = 0; i < table.length; ++i) {
    for (let j = 0; j < table[i].length; ++j) {
      if (table[i][j] !== 'A') continue;
      if (
        (toString(diagonal1(table, i, j)) === "MAS" && toString(diagonal2(table, i, j)) === "MAS") ||
        (toString(diagonal1(table, i, j)) === "MAS" && toString(diagonal3(table, i, j)) === "MAS") ||
        (toString(diagonal1(table, i, j)) === "MAS" && toString(diagonal4(table, i, j)) === "MAS") ||
        (toString(diagonal2(table, i, j)) === "MAS" && toString(diagonal3(table, i, j)) === "MAS") ||
        (toString(diagonal2(table, i, j)) === "MAS" && toString(diagonal4(table, i, j)) === "MAS") ||
        (toString(diagonal3(table, i, j)) === "MAS" && toString(diagonal4(table, i, j)) === "MAS")
      ) {
        count += 1;
      }
    }
  }
  return count
}

assert(countMasX(sampleTable) === 9);
console.log("Part 2: ", countMasX(parseInput(puzzleInput)))
