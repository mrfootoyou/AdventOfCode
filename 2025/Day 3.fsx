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
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    // Note: `assert` does not work in FSI, so must throw exception
    data
    |> Array.iter (fun s -> affirm (s.Length > 12) "Each row has more than 12 digits.")

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
    |> Array.sumBy (fun digits ->
        // grab the last 12 digits...
        let ds = digits[digits.Length - 12 ..]

        // for each digit d in ds, update it to be the _first largest_ value in digits[start..indexOf(d)].
        // Update start to be one after that index...
        let mutable start = 0

        for n = 0 to 12 - 1 do
            let dsn = digits.Length - 12 + n

            for i = dsn downto start do
                if digits[i] >= ds[n] then
                    ds[n] <- digits[i]
                    start <- i + 1

        // convert to 12-digit number
        ds |> Array.fold (fun acc d -> acc * 10UL + uint64 d) 0UL //|> dump
    )

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 357
executePuzzle "Part 1 finale" (fun () -> part1 data) 17142

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 3121910778619UL
executePuzzle "Part 2 finale" (fun () -> part2 data) 169935154100102UL
