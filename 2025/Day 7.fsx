// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Collections.Generic

type InputData = Grid<char>

let parseInput (text: string) : InputData =
    text |> String.splitAndTrim "\n" |> Grid.fromLines //|> tee Grid.printfn

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
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""

let sample2 = sample1

let data = getInput () |> parseData

let rec fire ((x,y as beam): Coordinates) (data: InputData) : Coordinates option =
    let next = Direction.offset beam Down
    match data |> Grid.getOrDefault next '#' with
    | '.' -> data |> fire next // continue down
    | '^' -> Some next // hit splitter
    | '#' -> None // off screen
    | c -> failwithf "Unexpected character '%c' at %A" c next

let part1 (data: InputData) =
    let start = data |> Grid.tryFind 'S' |> Option.get
    let mutable queue = Queue<Coordinates> [start]
    let mutable beams = HashSet<Coordinates>()

    while queue.Count > 0 do
        let beam = queue.Dequeue()
        match data |> fire beam with
        | None -> () // off screen
        | Some next -> 
            if beams.Add next then
                queue.Enqueue (Direction.offset next Left)
                queue.Enqueue (Direction.offset next Right)

    beams.Count

let part2 (data: InputData) =
    let start = data |> Grid.tryFind 'S' |> Option.get
    let mutable beams = Dictionary<Coordinates,int64>()

    let rec loop beam paths =
        match beams.TryGetValue beam with
        | true, subPaths ->
            paths + subPaths // already counted sub-paths
        | false, _ ->
            let paths =
                match data |> fire beam with
                | None -> paths + 1L
                | Some next -> 
                    let leftPaths = loop (Direction.offset next Left) 0L
                    let rightPaths = loop (Direction.offset next Right) 0L
                    paths + leftPaths + rightPaths
            beams[beam] <- paths
            paths

    loop start 0L

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 21
executePuzzle "Part 1 finale" (fun () -> part1 data) 1592

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 40L
executePuzzle "Part 2 finale" (fun () -> part2 data) 17921968177009L
