const assert = require("assert");
const fs = require("fs");

function parseLine(line) {
    const [name, rest] = line.split(": ");
    const id = Number.parseInt(name.split(" ")[1]);
    const tosses = rest.split("; ").map(t => t.split(", ").map(v => {
        const [number, color] = v.split(" ");
        return [color, Number.parseInt(number)];
    }));
    return [id, tosses];
}
const trainingInput = `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`;

const problemInput = fs.readFileSync("input.txt").toString();


/// Part 1
const maxValues = {
    red: 12,
    green: 13,
    blue: 14
}

function isLegalToss(toss) {
    return toss.every(([color, number]) => maxValues[color] >= number);
}

function isLegalGame(game) {
    return game[1].every(isLegalToss);
}

function sumIds(input) {
    const lines = input.split("\n");
    const games = lines.map(parseLine);
    const legalGames = games.filter(isLegalGame);
    return legalGames.reduce((acc, game) => game[0] + acc , 0);
}


assert(sumIds(trainingInput) === 8);


console.log(sumIds(problemInput));


/// Part 2
function minimumSet(game) {
    let result = { red: 0, green: 0, blue: 0 };
    for (let toss of game[1]) {
        for (let [color, number] of toss) {
            result[color] = Math.max(number, result[color]);
        }
        
    }

    return Object.values(result);
}

function powerOfGame(game) {
    return minimumSet(game).reduce((acc, current) => acc*current, 1);
}

function sumOfPowers(input) {
    const games = input.split("\n").map(parseLine);
    const powers = games.map(powerOfGame);
    return powers.reduce((acc, current) => acc + current, 0);
}

assert(sumOfPowers(trainingInput) === 2286);

console.log(sumOfPowers(problemInput));
