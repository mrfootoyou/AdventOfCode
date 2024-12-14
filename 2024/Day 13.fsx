// https://adventofcode.com/2024/day/13
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

[<Literal>]
let CostA = 3L

[<Literal>]
let CostB = 1L

type Machine =
    { ax: int64
      ay: int64
      bx: int64
      by: int64
      px: int64
      py: int64 }

module Machine =
    let inline pushA (x, y) times machine =
        (x + machine.ax * (int64) times, y + machine.ay * (int64) times)

    let inline pushB (x, y) times machine =
        (x + machine.bx * (int64) times, y + machine.by * (int64) times)

    let inline dist (x, y) machine = (machine.px - x, machine.py - y)

type InputData = Machine[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Seq.map (fun s ->
        let m = Regex.Match(s, @"X[+=](\d+), Y[+=](\d+)")
        m.Groups[1].Value |> Int64.Parse, m.Groups[2].Value |> Int64.Parse)
    |> Seq.chunkBySize 3
    |> Seq.map (function
        | [| (ax, ay); (bx, by); (px, py) |] ->
            { ax = ax
              ay = ay
              bx = bx
              by = by
              px = px
              py = py }
        | _ -> failwith "Unexpected input")
    |> Seq.toArray
// |> dump

let validateAssumptions (data: InputData) =
    data
    |> Array.iter (function
        | { ax = n }
        | { ay = n }
        | { bx = n }
        | { by = n } as m when n <= 0L || n > 99L -> failwithf "Bad assumption: %A" m
        | { px = n }
        | { py = n } as m when n <= 0L -> failwithf "Bad assumption: %A" m
        | _ -> ())

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

let minimize m =
    // Solution from Copilot:
    // To solve for a and b given the equations:
    //
    //    px = (a * ax) + (b * bx)
    //    py = (a * ay) + (b * by)
    //
    // You can use linear algebra to solve this system of equations.
    // ...
    let a = (m.by * m.px - m.bx * m.py) / (m.ax * m.by - m.ay * m.bx)
    let b = (m.ax * m.py - m.ay * m.px) / (m.ax * m.by - m.ay * m.bx)

    if m.px = a * m.ax + b * m.bx && m.py = a * m.ay + b * m.by then
        a * CostA + b * CostB
    else
        0

let minimizeAll (data: InputData) =
    data |> Array.Parallel.map minimize |> Array.sum

let part1 (data: InputData) = data |> minimizeAll

let part2 (data: InputData) =
    data
    |> Array.map (fun m ->
        { m with
            px = m.px + 10000000000000L
            py = m.py + 10000000000000L })
    |> minimizeAll

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 480L
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 28138L

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 875318608908L
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 108394825772874L
