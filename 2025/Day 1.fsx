// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = (int)[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        match s[0] with
        | 'L' -> -Int32.Parse(s.AsSpan().Slice 1)
        | 'R' -> +Int32.Parse(s.AsSpan().Slice 1)
        | _ -> failwithf "Unexpected input: %s" s)
// |> echo

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if data |> Array.exists ((=) 0) then
        failwith "Bad assumption: contains zero rotation"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 (data: InputData) =
    let _, zeros =
        data
        // convert to positive rotation
        |> Seq.map (function
            | dp when dp < 0 -> dp % 100 + 100 
            | dp -> dp)
        // rotate clockwise and count zeros
        |> Seq.fold
            (fun (pos, zeros) dp ->
                match (pos + dp) % 100 with
                | 0 as newPos -> newPos, zeros + 1
                | newPos -> newPos, zeros)
            (50, 0)

    zeros

let part2 (data: InputData) =
    let _, zeros =
        data
        |> Seq.fold
            (fun (pos, zeros) dp ->
                match dp with
                | dp when dp >= 0 ->
                    //rotate clockwise
                    let newPos = (pos + dp) % 100
                    let zs = (pos + dp) / 100
                    newPos, zeros + zs

                | dp -> // negative
                    //rotate counter-clockwise
                    match pos + dp with 
                    | newPos when newPos > 0 -> newPos, zeros // didn't cross zero
                    | 0 -> 0, zeros + 1 // landed on zero
                    | n -> 
                        // crossed zero one or more times
                        let newPos = (100 - -n % 100) % 100
                        let zs = (if pos = 0 then 0 else 1) + -n / 100
                        newPos, zeros + zs
                    )
            (50, 0)

    zeros

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 3
executePuzzle "Part 1 finale" (fun () -> part1 data) 964

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 6
executePuzzle "Part 2 finale" (fun () -> part2 data) 5872
