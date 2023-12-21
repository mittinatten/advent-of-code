const assert = require("assert");
const fs = require("fs");

const validNumbers = [
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "zero"]

function getNumber(line) {
    let first, last;
    for (let i = 0; i < line.length; ++i) {
        first = validNumbers.findIndex(number => line.substr(i).startsWith(number))
        if (first !== -1) break;
    }
    for (let i = line.length - 1; i >= 0; --i) {
        last = validNumbers.findIndex(number => line.substr(i).startsWith(number))
        if (last !== -1) break;
    }

    return (first % 10 + 1) * 10 + (last % 10 + 1);
}

const trainingInput = [
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
]

//const expectedNumbers = [29, 83, 13, 24, 42, 14, 76];

function sumLines(lines) {
    return lines.map(getNumber).reduce((acc, curr) => acc + curr, 0);
}

assert(sumLines(trainingInput) === 281);

const problemInput = fs.readFileSync("input.txt").toString().split("\n");

console.log(sumLines(problemInput));

