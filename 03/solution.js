const assert = require("assert");
const fs = require("fs");

const trainingInput = `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`;
const problemInput = fs.readFileSync("input.txt").toString();

const numbers = "0123456789";

function parseLine(line) {
    const result = {
        numbers: [],
        symbols: [],
        line
    }

    let prevIsNumber = false;
    for (let i = 0; i < line.length; ++i) {
        if (line[i] === ".") {           
            prevIsNumber = false;
        } else if (numbers.includes(line[i])) {
            if (prevIsNumber) {
                result.numbers.at(-1)[1] = i;
            } else {
                result.numbers.push([i,i]);
                prevIsNumber = true;
            }
        } else {
            result.symbols.push(i);
            prevIsNumber = false;
        }
    }

    return result
}

function parseSchematic(input) {
    return input.split("\n").map(parseLine);
}

function isNumberPart(lines, lineNumber, [start, end]) {
    if (lineNumber > 0) {
        const lineBefore = lines[lineNumber - 1];
        if (lineBefore.symbols.some(position => position >= start - 1 && position <= end + 1)) {
            return true;
        }
    }

    const currentLine = lines[lineNumber];
    if (start > 0 && currentLine.symbols.includes(start - 1)) {
        return true;
    }

    if (end < currentLine.line.length - 1 && currentLine.symbols.includes(end + 1)) {
        return true;
    }

    if (lineNumber < lines.length - 1) {
        const nextLine = lines[lineNumber + 1];
        if (nextLine.symbols.some(position => position >= start - 1 && position <= end + 1)) {
            return true;
        }
    }
    
    return false;
}

function getNumber(line, [start, end]) {
    return Number.parseInt(line.substring(start, end + 1));
}

function getParts(input) {
    const lines = parseSchematic(input);
    let parts = [];
    for (let i = 0; i < lines.length; ++i) {
        parts = parts.concat(
            lines[i].numbers
                .filter(number => isNumberPart(lines, i, number))
                .map(number => getNumber(lines[i].line, number)));
       
    }
    return parts;
}

// returns 0 if not a gear
function getGearRatio(lines, lineNumber, position) {
    let neighbors = []
    if (lineNumber > 0) {
        const lineBefore = lines[lineNumber - 1];
        for (let neighbor of lineBefore.numbers
                 .filter(([start, end]) => position >= start - 1 && position <= end + 1)) {
            neighbors.push(getNumber(lineBefore.line, neighbor));
        }        
    }

    const currentLine = lines[lineNumber];
    for (let [start, end] of currentLine.numbers) {
        if (end === position - 1 || start === position + 1) {
            neighbors.push(getNumber(currentLine.line, [start, end]));
        }
    }

    if (lineNumber < lines.length - 1) {
        const nextLine = lines[lineNumber + 1];
        for (let neighbor of nextLine.numbers
                 .filter(([start, end]) => position >= start - 1 && position <= end + 1)) {
            neighbors.push(getNumber(nextLine.line, neighbor));
        }        
    }

    if (neighbors.length === 2) {
        return neighbors[0] * neighbors[1];
    }
    return 0;
}

function getGearRatioSum(input) {
    const lines = parseSchematic(input);
    let sum = 0
    for (let i = 0; i < lines.length; ++i) {
        let { symbols, line } = lines[i];
        for (let symbol of symbols) {
            if (line[symbol] === "*") {
                sum += getGearRatio(lines, i, symbol);;
            }
        }
    }

    return sum;
}

assert(getParts(trainingInput).reduce((acc, current) => acc + current, 0) === 4361);

assert(getGearRatioSum(trainingInput) === 467835);
assert(getGearRatioSum("2*3") === 6);
assert(getGearRatioSum(`1..*..6
..2.3..`) === 6);

console.log(getParts(problemInput).reduce((acc, current) => acc + current, 0));
console.log(getGearRatioSum(problemInput));
