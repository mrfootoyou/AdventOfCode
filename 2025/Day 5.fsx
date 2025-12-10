// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = (uint64 * uint64)[] * uint64[]

let parseInput (text: string) : InputData =
    let parts = text.Trim().Replace("\r", "").Split("\n\n")

    let range =
        parts[0]
        |> String.splitAndTrim "\n"
        |> Seq.map (String.split "-")
        |> Seq.map (fun arr -> UInt64.Parse(arr[0]), UInt64.Parse(arr[1]))
        |> Seq.toArray

    let ingredients = parts[1] |> String.splitAndTrim "\n" |> Array.map UInt64.Parse
    (range, ingredients) // |> echo

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    // affirm (Grid.width data = Grid.height data) "grid is square"
    ()

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 ((fresh, ingredients): InputData) =
    let inRange (value: uint64) (start: uint64, end_: uint64) = start <= value && value <= end_

    ingredients
    |> Seq.where (fun ingredient -> fresh |> Array.exists (inRange ingredient))
    |> Seq.length

let part2 ((fresh, _): InputData) =
    seq {
        // sort by start then iterate looking for gaps
        //        12345678901234567890
        // 3-5  : ..###...............
        // 10-14: .........#####......
        // 12-18: ...........#######..
        // 16-20: ...............#####
        // Ans:   ..xxx....xxxxxxxxxxx
        fresh |> Array.sortInPlaceWith (fun (s1, _) (s2, _) -> compare s1 s2)

        let mutable rs, re = fresh[0]

        for i = 1 to fresh.Length - 1 do
            let s, e = fresh[i]

            if s <= re then
                // overlap with previous
                re <- max re e
            else
                // gap
                yield (rs, re)
                rs <- s
                re <- e

        yield (rs, re)
    }
    |> Seq.sumBy (fun (s, e) -> e - s + 1UL)

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 3
executePuzzle "Part 1 finale" (fun () -> part1 data) 761

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 14UL
executePuzzle "Part 2 finale" (fun () -> part2 data) 345755049374932UL
