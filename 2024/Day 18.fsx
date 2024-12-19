// https://adventofcode.com/2024/day/18
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharpHelpers

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

    let rec bfs (queue: Queue<_>) visited minDist =
        match queue.TryDequeue() with
        | false, _ -> minDist // done. exhausted queue
        | true, (x, y, dist) ->
            if x = size && y = size then
                // reached the exit. update the minimum distance
                bfs queue visited (min minDist dist)
            else
                match visited |> Map.tryFind (x, y) with
                | Some d when d <= dist ->
                    // already visited with a shorter distance
                    bfs queue visited minDist
                | _ ->
                    let visited = visited |> Map.add (x, y) dist

                    // add neighbors to the queue
                    for (nx, ny) in [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ] do
                        if g |> Grid.itemOrDefault nx ny ' ' = Safe then
                            queue.Enqueue((nx, ny, dist + 1))

                    bfs queue visited minDist

    let queue = Queue([ (0, 0, 0) ])
    bfs queue Map.empty Int32.MaxValue

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
