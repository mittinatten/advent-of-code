const assert = require("assert");
const fs = require("fs");

const trainingInput = `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4`;

const problemInput = fs.readFileSync("input.txt").toString();

const seedToSoil = new Map();
const soilToFertilizer = new Map();
const fertilizerToWater = new Map();
const waterToLight = new Map();
const lightToTemperature = new Map();
const temperatureToHumidity = new Map();
const humidityToLocation = new Map();

let currentLine = 0;

function parseNextMap(lines, map) {
    const label = lines[currentLine++];
    const [from, to] = label.replace(" map:", "").split("-to-");
    const maps = [];

    while (currentLine < lines.length && lines[currentLine].trim() !== "") {
        const [destination, source, length] =
              lines[currentLine].split(/\s+/).map(x => Number.parseInt(x));

        maps.push({ source, destination, length });
        ++currentLine;
    }

    map[from] = {
        to,
        maps,
    };
}

function parseInput(input) {
    const map = {};
    currentLine = 0;
    const lines = input.split("\n");

    const seeds = lines[0].split(": ").pop().split(" ").map(s => Number.parseInt(s));
        
    ++currentLine;
    while (lines[currentLine].trim() === "") {
        ++currentLine;
    }

    while (currentLine < lines.length) {
        parseNextMap(lines, map);
        ++currentLine;
    }

    return { seeds, map };
}

function getDestination(maps, source) {
    const map = maps.find(m => m.source <= source && m.source + m.length >= source);
    if (map) {
        return map.destination + (source - map.source);
    }
    return source;
}

function findLocation(seed, map) {
    let nextKey = "seed";
    let value = seed;
    do {
        const maps = map[nextKey].maps
        value = getDestination(map[nextKey].maps, value);
        nextKey = map[nextKey].to;
    } while (nextKey !== "location");

    return value;
}

function findClosestLocation(input) {
    const { seeds, map } = parseInput(input);
    
    const locations = seeds.map(seed => findLocation(seed, map));
    return Math.min(...locations);
}
assert(findClosestLocation(trainingInput) === 35);
console.log(findClosestLocation(problemInput));
