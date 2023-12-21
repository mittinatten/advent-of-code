const assert = require("assert");
const fs = require("fs");

function getNumber(line) {
    const match = line.match(/^[^\d]*(\d).*(\d)[^\d]*$/);
    if (match) {
        return Number.parseInt(match[1]) * 10 + Number.parseInt(match[2]);
    }

    const match2 = line.match(/.*(\d).*/);
    if (match2) {
        const val = Number.parseInt(match2[1]);
        return val * 10 + val;
    }

    throw Error("Can't parse string '" + line + "'");
}

function sumLines(lines) {
    return lines.map(getNumber).reduce((acc, curr) => acc + curr, 0);
}

const trainingInput = ["1abc2",
             "pqr3stu8vwx",
             "a1b2c3d4e5f",
               "treb7uchet"];
assert(sumLines(trainingInput) === 142);

const problemInput = fs.readFileSync("input.txt").toString().split("\n");

console.log(sumLines(problemInput));


