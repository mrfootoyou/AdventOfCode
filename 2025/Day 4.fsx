// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = Grid<char>

let parseInput (text: string) : InputData =
    text |> String.splitAndTrim "\n" |> Grid.fromLines //|> tee Grid.printfn

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    affirm (Grid.width data = Grid.height data) "grid is square"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""

let sample2 = sample1

let data = getInput () |> parseData

let directions: ValueTuple<int, int>[] =
    [| (1, 1); (1, 0); (1, -1); (0, 1); (0, -1); (-1, 1); (-1, 0); (-1, -1) |]

let canRemove (x: int, y: int) (data: InputData) =
    let mutable count = 0
    let mutable i = 0
    let directions = directions.AsSpan()

    while count <= 3 && i < directions.Length do
        let struct (dx, dy) = directions[i]
        i <- i + 1

        match data |> Grid.itemOrDefault (x + dx) (y + dy) '.' with
        | '@' -> count <- count + 1
        | _ -> ()

    count <= 3

let part1 (data: InputData) =
    data
    |> Grid.fold
        (fun removed coord value ->
            match value with
            | '@' when data |> canRemove coord -> removed + 1
            | _ -> removed)
        0

let part2 (data: InputData) =
    let mutable data = data
    let mutable next = data |> Grid.clone
    let mutable totalRemoved = 0
    let mutable changed = true

    while changed do
        let removed =
            data
            |> Grid.fold
                (fun removed (x, y as coord) value ->
                    match value with
                    | '@' when data |> canRemove coord ->
                        next |> Grid.set x y '.'
                        removed + 1
                    | _ -> removed)
                0

        if removed = 0 then
            changed <- false
        else
            next |> Grid.copy data
            data <- next
            totalRemoved <- totalRemoved + removed

    totalRemoved

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 13
executePuzzle "Part 1 finale" (fun () -> part1 data) 1351

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 43
executePuzzle "Part 2 finale" (fun () -> part2 data) 8345
