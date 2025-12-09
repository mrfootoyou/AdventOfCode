// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = int[][]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Grid.fromLines
    |> Grid.map (fun _ c -> digitToInt c)
// |> tee Grid.printfn

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "Bad assumption: xxx"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
987654321111111
811111111111119
234234234234278
818181911112111
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 (data: InputData) =
    data
    |> Array.sumBy (fun row ->
        let row = row.AsSpan()
        let mutable maxI, maxJolts = 0, 0

        for i = 0 to row.Length - 2 do
            if row[i] >= maxI then // optimization
                for j = i + 1 to row.Length - 1 do
                    let jolts = row[i] * 10 + row[j]

                    if jolts > maxJolts then
                        maxI <- row[i]
                        maxJolts <- jolts

        maxJolts)

let part2 (data: InputData) =
    data
    |> Array.Parallel.sumBy (fun row ->
        let mutable maxA, maxJolts = 0, 0UL

        let row = row.AsSpan()

        for a = 0 to row.Length - 12 do
            if row[a] >= maxA then // optimization
                let jolts = row[a]

                for b = a + 1 to row.Length - 11 do
                    let jolts = jolts * 10 + row[b]

                    for c = b + 1 to row.Length - 10 do
                        let jolts = jolts * 10 + row[c]

                        for d = c + 1 to row.Length - 9 do
                            let jolts = jolts * 10 + row[d]

                            for e = d + 1 to row.Length - 8 do
                                let jolts = jolts * 10 + row[e]

                                for f = e + 1 to row.Length - 7 do
                                    let jolts = jolts * 10 + row[f]

                                    for g = f + 1 to row.Length - 6 do
                                        let jolts = jolts * 10 + row[g]

                                        for h = g + 1 to row.Length - 5 do
                                            let jolts = jolts * 10 + row[h]

                                            for i = h + 1 to row.Length - 4 do
                                                let jolts = jolts * 10 + row[i] |> uint64

                                                for j = i + 1 to row.Length - 3 do
                                                    let jolts = jolts * 10UL + uint64 row[j]

                                                    for k = j + 1 to row.Length - 2 do
                                                        let jolts = jolts * 10UL + uint64 row[k]

                                                        for l = k + 1 to row.Length - 1 do
                                                            let jolts = jolts * 10UL + uint64 row[l]

                                                            if jolts > maxJolts then
                                                                maxJolts <- jolts
                                                                maxA <- row[a]

        maxJolts)

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 357
executePuzzle "Part 1 finale" (fun () -> part1 data) 17142

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 3121910778619UL
executePuzzle "Part 2 finale" (fun () -> part2 data) 0UL
