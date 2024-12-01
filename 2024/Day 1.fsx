// https://adventofcode.com/2024/day/X
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type InputData = (int * int)[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        match s.Split("   ") with
        | [| a; b |] -> (Int32.Parse(a), Int32.Parse(b))
        | _ -> failwithf "Unexpected input: %s" s)
// |> tee (printfn "%A")

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
3   4
4   3
2   5
1   3
3   9
3   3
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 (data: InputData) =
    //
    let (a, b) = data |> Array.unzip
    a |> Array.sortInPlace
    b |> Array.sortInPlace
    Array.zip a b |> Array.sumBy ((fun (a, b) -> Math.Abs(a - b)))

let part2 (data: InputData) =
    //
    let (a, b) = data |> Array.unzip

    let lookup =
        b
        |> Array.fold
            (fun m b ->
                m
                |> Map.change b (function
                    | Some x -> Some(x + 1)
                    | _ -> Some 1))
            Map.empty

    a
    |> Array.sumBy (fun a -> a * (lookup |> Map.tryFind a |> Option.defaultValue 0))

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 11
executePuzzle "Part 1 finale" (fun () -> part1 data) 2344935

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 31
executePuzzle "Part 2 finale" (fun () -> part2 data) 27647262
