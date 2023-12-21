const assert = require("assert");
const fs = require("fs");

const trainingInput = `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`;

const problemInput = fs.readFileSync("input.txt").toString();

function parseCard(card) {
    const [label, content] = card.split(/:\s+/);
    const [winningPart, yourPart] = content.split(/ \|\s+/);
    const winning = winningPart.split(/\s+/).map(n => Number.parseInt(n));
    const yours = yourPart.split(/\s+/).map(n => Number.parseInt(n));
    return [winning, yours];
}

function scoreCard(card) {
    const [winning, yours] = parseCard(card);
    const set = new Set(winning);
    const score = yours.reduce(
        (acc, n) =>
            set.has(n)
            ? (acc == 0 ? 1 : 2 * acc)
            : acc,
        0);
    return score;
}

function sumScore(input) {
    return input.split("\n").map(scoreCard).reduce((acc, score) => score + acc, 0)
}

assert(sumScore(trainingInput) === 13);
console.log(sumScore(problemInput));

function scoreCard2(card) {
    const [winning, yours] = parseCard(card);
    const set = new Set(winning);
    const score = yours.reduce(
        (acc, n) => set.has(n) ? acc + 1 : acc,
        0
    );
    return score;
}

function sumScore2(input) {
    const cards = input.split("\n");
    const ncards = cards.length;
    const initialScores = cards.map(scoreCard2);
    const finalScores = new Array(ncards).fill(1);
    for (let i = 0; i < ncards; ++i) {        
        const scoreSoFar = finalScores[i];
        for (let j = 1; j <= initialScores[i] && i + j < ncards; ++j) {
            finalScores[i + j] += scoreSoFar;
        }
    }
    return finalScores.reduce((acc, s) => acc + s, 0);
}

assert(sumScore2(trainingInput) === 30);
console.log(sumScore2(problemInput));
