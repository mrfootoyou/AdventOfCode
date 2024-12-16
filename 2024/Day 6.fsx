// https://adventofcode.com/2024/day/6
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers
open Direction

[<Literal>]
let Rock = '#'

[<Literal>]
let Open = '.'

[<Literal>]
let Visited = 'X'

type InputData = Grid<char> * Grid.Coordinates

let parseInput (text: string) : InputData =
    let map = text |> String.splitAndTrim "\n" |> Grid.fromLines
    let pos = map |> Grid.find (toArrow Up)
    map, pos //|> echo

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

let part1 ((g, (x, y)): InputData) =
    let g = g |> Grid.clone

    let rec move x y dir =
        let (Delta(dx, dy)) = dir
        let (nx, ny) = x + dx, y + dy
        g |> Grid.set x y Visited

        match g |> Grid.tryItemV nx ny with
        | ValueSome Rock ->
            // turn right, don't move
            let dir = dir |> turnRight
            move x y dir
        | ValueSome _ ->
            // move forward, don't turn
            move nx ny dir
        | ValueNone ->
            // out of bounds
            ()

    let dir = g |> Grid.item x y |> fromArrow
    move x y dir
    g |> Grid.fold (fun s _ c -> if c = Visited then s + 1 else s) 0

let part2 ((g, ((x, y) as origin)): InputData) =
    let g = g |> Grid.clone

    let rec hasLoop x y dir path =
        if path |> Set.contains (x, y, dir) then
            true
        else
            let m = path |> Set.add (x, y, dir)
            let (Delta(dx, dy)) = dir
            let (nx, ny) = x + dx, y + dy

            match g |> Grid.tryItemV nx ny with
            | ValueSome Rock ->
                // turn right
                hasLoop x y (dir |> turnRight) m
            | ValueSome _ ->
                // move forward
                hasLoop nx ny dir m
            | ValueNone ->
                // out of bounds (no loop)
                false

    let rec move x y dir path positions =
        let path = path |> Set.add (x, y, dir)
        let (Delta(dx, dy)) = dir
        let (nx, ny) as next = x + dx, y + dy

        match g |> Grid.tryItemV nx ny with
        | ValueSome Rock ->
            // turn right
            move x y (dir |> turnRight) path positions
        | ValueSome v ->
            // place a rock in next position and see what happens...
            g |> Grid.set nx ny Rock

            let positions =
                if positions |> Map.containsKey next then
                    positions // already checked
                elif hasLoop x y (dir |> turnRight) path then
                    positions |> Map.add next true
                else
                    positions |> Map.add next false

            // remove rock and move forward
            g |> Grid.set nx ny v
            move nx ny dir path positions
        | ValueNone ->
            // out of bounds (done)
            positions

    let dir = g |> Grid.item x y |> fromArrow
    let positions = move x y dir Set.empty Map.empty
    positions |> Map.values |> Seq.where id |> Seq.length

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 41
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 5080

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 6
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 1919
