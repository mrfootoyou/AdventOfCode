// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Text.RegularExpressions

type InputData = string[]

let parseInput (text: string) : InputData =
    text.Replace("\r", "")
    |> String.splitO "\n" StringSplitOptions.RemoveEmptyEntries

let validateAssumptions (lines: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    affirm (Array.TrueForAll(lines, fun line -> line.Length = lines[0].Length)) "all lines are same length"
    ()

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData (
        [|
           // use quotes to preserve trailing spacing
           "123 328  51 64 "
           " 45 64  387 23 "
           "  6 98  215 314"
           "*   +   *   +  " |]
        |> String.concat "\n"
    )

let sample2 = sample1

let data = getInput () |> parseData

let part1 (lines: InputData) : uint64 =
    let operators =
        lines[lines.Length - 1] |> String.splitAndTrim " " |> Array.map (fun s -> s[0])

    let numbers =
        lines
        |> Seq.take (lines.Length - 1)
        |> Seq.map (String.splitAndTrim " ")
        |> Seq.map (Array.map UInt64.Parse)
        |> Seq.toArray
        |> Array.transpose

    Array.zip numbers operators
    |> Array.map (fun (ns, op) ->
        match op with
        | '+' -> Array.sum ns
        | '*' -> Array.fold (*) 1UL ns
        | _ -> failwithf "Unknown operator: %c" op)
    |> Array.sum

type State =
    | Starting
    | ReadingNumber of int * uint64 list
    | NextNumber of uint64 list

[<return: Struct>]
let (|Digit|_|) (c: char) =
    if Char.IsDigit c then
        ValueSome(int c - int '0')
    else
        ValueNone

let part2 (lines: InputData) =
    let grid = lines |> Grid.fromLines |> Grid.rotate 90 // |> tee Grid.printfn
    // Example grid (with spaces as ·):
    //  +----+
    //  |··4·|
    //  |431·|
    //  |623+| -> 4 + 431 + 623
    //  |····|
    //  |175·|
    //  |581·|
    //  |·32*| -> 175 * 581 * 32
    //  |····|
    //  |8···|
    //  |248·|
    //  |369+| -> 8 + 248 + 369
    //  |····|
    //  |356·|
    //  |24··|
    //  |1··*| -> 356 * 24 * 1
    //  +----+
    seq {
        let mutable state = Starting

        for pos, c in grid |> Grid.toCoordSeq do
            match state, c with
            | Starting, ' ' -> () // stay
            | Starting, Digit c -> state <- ReadingNumber(c, [])

            | ReadingNumber(n, ns), Digit c -> state <- ReadingNumber(n * 10 + c, ns)
            | ReadingNumber(n, ns), ' ' -> state <- NextNumber(uint64 n :: ns)

            | NextNumber ns, Digit c -> state <- ReadingNumber(c, ns)
            | NextNumber _, ' ' -> () // stay

            | ReadingNumber(n, ns), '+' ->
                yield uint64 n + List.sum ns
                state <- Starting
            | NextNumber ns, '+' ->
                yield List.sum ns
                state <- Starting

            | ReadingNumber(n, ns), '*' ->
                yield uint64 n * List.fold (*) 1UL ns
                state <- Starting
            | NextNumber ns, '*' ->
                yield List.fold (*) 1UL ns
                state <- Starting

            | _ -> failwith $"Unexpected {state} at {pos:A}: '{c}'"
    }
    |> Seq.sum

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 4277556UL
executePuzzle "Part 1 finale" (fun () -> part1 data) 6299564383938UL

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 3263827UL
executePuzzle "Part 2 finale" (fun () -> part2 data) 11950004808442UL
