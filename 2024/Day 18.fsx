// https://adventofcode.com/2024/day/18
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers
open Direction

type InputData = Coordinates[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        let ss = s.Split(',')
        ss[0] |> Int32.Parse, ss[1] |> Int32.Parse)
// |> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    // if data |> Array.exists () then
    //     failwith "bad assumption"
    ()

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

[<Literal>]
let Safe = '.'

[<Literal>]
let Corrupt = '#'

let solve size bytes (data: InputData) =
    let g = Grid.create (size + 1) (size + 1) Safe
    data |> Seq.take bytes |> Seq.iter (fun (x, y) -> g |> Grid.set x y Corrupt)
    // g |> Grid.printfn

    let canMove x y = g |> Grid.itemOrDefault x y ' ' = Safe

    let rec loop minDist visited next =
        match next with
        | [] -> minDist // done
        | (x, y, dist) :: next ->
            if x = size && y = size then
                // found exit
                loop (min minDist dist) visited next
            else
                match visited |> Map.tryFind (x, y) with
                | Some otherDist when otherDist <= dist ->
                    // other path is shorter
                    loop minDist visited next
                | _ ->
                    let visited = visited |> Map.add (x, y) dist

                    // try to move in all directions
                    next
                    @ [ if canMove (x + 1) y then
                            (x + 1, y, dist + 1)
                        if canMove (x - 1) y then
                            (x - 1, y, dist + 1)
                        if canMove x (y + 1) then
                            (x, y + 1, dist + 1)
                        if canMove x (y - 1) then
                            (x, y - 1, dist + 1) ]
                    |> loop minDist visited

    loop Int32.MaxValue Map.empty [ (0, 0, 0) ]

let part1 size bytes (data: InputData) = solve size bytes data

let part2 size start (data: InputData) =
    // use binary search to home-in on the last passable byte...
    let mutable lastPassableByte = 0

    binarySearch (start + 1) data.Length (fun bytes ->
        match solve size bytes data with
        | Int32.MaxValue ->
            // blocked. search with fewer bytes
            -1
        | _ ->
            // passable. search with more bytes
            lastPassableByte <- bytes
            +1)
    |> ignore

    data[lastPassableByte]

executePuzzle "Part 1 sample" (fun () -> part1 6 12 sample1) 22
executePuzzle "Part 1 finale" (fun () -> part1 70 1024 data.Value) 264

executePuzzle "Part 2 sample" (fun () -> part2 6 12 sample1) (6, 1)
executePuzzle "Part 2 finale" (fun () -> part2 70 1024 data.Value) (41, 26)
