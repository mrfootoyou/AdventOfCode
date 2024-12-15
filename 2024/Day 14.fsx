// https://adventofcode.com/2024/day/14
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type Record = (Grid.Coordinates * (int * int))
type InputData = Record[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        let m = Regex.Match(s, @"p=([^,]+),([^ ]+?) v=([^,]+),(.+)$")

        (m.Groups[1].Value |> Int32.Parse, m.Groups[2].Value |> Int32.Parse),
        (m.Groups[3].Value |> Int32.Parse, m.Groups[4].Value |> Int32.Parse))
// |> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

let inline addWrap x dx w =
    match (x + dx) % w with
    | nx when nx < 0 -> nx + w
    | nx -> nx

let part1 w h (data: InputData) =
    let secs = 100
    let g = Grid.create w h 0

    data
    |> Seq.iter (fun ((x, y), (dx, dy)) ->
        let x = addWrap x (secs * dx) w
        let y = addWrap y (secs * dy) h
        let c = g |> Grid.item x y
        g |> Grid.set x y (c + 1))

    // Grid.printfn g
    let (mx, my) = w / 2, h / 2

    let (q1, q2, q3, q4) =
        g
        |> Grid.fold
            (fun (q1, q2, q3, q4) (x, y) c ->
                match x, y with
                | x, y when x < mx && y < my -> (q1 + c, q2, q3, q4)
                | x, y when x < mx && y > my -> (q1, q2 + c, q3, q4)
                | x, y when x > mx && y < my -> (q1, q2, q3 + c, q4)
                | x, y when x > mx && y > my -> (q1, q2, q3, q4 + c)
                | _ -> (q1, q2, q3, q4))
            (0, 0, 0, 0)

    q1 * q2 * q3 * q4

let part2 w h (data: InputData) answer =
    let pos = data |> Array.map fst
    let delta = data |> Array.map snd
    let startTime = answer // 0
    let stepSize = 1
    let g = Grid.create w h 0
    let g2 = Grid.create w h ' '

    for i = 0 to pos.Length - 1 do
        let (x, y) = pos[i]
        let (dx, dy) = delta[i]
        let x = addWrap x (startTime * dx) w
        let y = addWrap y (startTime * dy) h
        g[y][x] <- g[y][x] + 1
        g2[y][x] <- '#'
        pos[i] <- (x, y)

    let search threshold =
        let mutable time = startTime

        while time < 100_000 do
            time <- time + stepSize

            for i = 0 to pos.Length - 1 do
                let (x, y) = pos[i]
                let (dx, dy) = delta[i]
                let nx = addWrap x (stepSize * dx) w
                let ny = addWrap y (stepSize * dy) h
                pos[i] <- (nx, ny)

                let c = (g |> Grid.item x y) - 1
                let nc = (g |> Grid.item nx ny) + 1
                g |> Grid.set x y c
                g |> Grid.set nx ny nc

                if c = 0 then
                    g2 |> Grid.set x y ' '

                if nc = 1 then
                    g2 |> Grid.set nx ny '#'

            if float (pos |> Array.sumBy (fun (x, y) -> x + y)) / float pos.Length > threshold then
                // Console.SetCursorPosition(0, 0)
                printfn "Step: %d" time
                g2 |> Grid.printfn

    if answer = 0 then
        search 115.0

    float (pos |> Array.sumBy (fun (x, y) -> x + y)) / float pos.Length

//answer

executePuzzle "Part 1 sample" (fun () -> part1 11 7 sample1) 12
executePuzzle "Part 1 finale" (fun () -> part1 101 103 data.Value) 225552000

// executePuzzle "Part 2 sample" (fun () -> part2 sample1) 0
executePuzzle "Part 2 finale" (fun () -> part2 101 103 data.Value 7371) 7371
