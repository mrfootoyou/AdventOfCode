// https://adventofcode.com/2024/day/X
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers
open FSharpHelpers.Direction

type InputData = string[]

let parseInput (text: string) : InputData = text |> String.splitAndTrim "\n" //|> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
029A
980A
179A
456A
379A
"""

let sample2 = sample1

let data =
    let rawData = getInput ()
    lazy (rawData |> parseData)

let numPad =
    """
#####
#789#
#456#
#123#
# 0A#
#####
"""
    |> String.splitAndTrim "\n"
    |> Grid.fromLines

let arrowsPad =
    """
#####
# ^A#
#<v>#
#####
"""
    |> String.splitAndTrim "\n"
    |> Grid.fromLines

let plan keypad controllingArrows keys =
    seq {
        let mutable (x, y) = if keypad |> Grid.item 3 1 = 'A' then (3, 1) else (3, 4)
        let (spaceX, spaceY) = 1, y
        // assert (keypad |> Grid.item x y = 'A')
        // assert (keypad |> Grid.item spaceX spaceY = ' ')

        for c in keys do
            // find the next button...
            let (nx, ny) = keypad |> Grid.find c
            let (dx, dy) = nx - x, ny - y

            // move to the button...
            // Moving from the Space row to the Space column (or vice versa) requires
            // us to move vertically (or horizontally) first to avoid passing through the
            // empty space.
            if y = spaceY && nx = spaceX then
                for _ in 1 .. abs dy do
                    yield if dy < 0 then '^' else 'v'

                for _ in 1 .. abs dx do
                    yield if dx < 0 then '<' else '>'

            else if x = spaceX && ny = spaceY then
                for _ in 1 .. abs dx do
                    yield if dx < 0 then '<' else '>'

                for _ in 1 .. abs dy do
                    yield if dy < 0 then '^' else 'v'
            else if
                // We can move either vertically or horizontally first.
                // When controlling arrows, prefer moving right (+dx) or up (-dy) last
                // so that moving to the 'A' button is only one move away.
                controllingArrows && dx > 0
            then
                for _ in 1 .. abs dy do
                    yield if dy < 0 then '^' else 'v'

                for _ in 1 .. abs dx do
                    yield if dx < 0 then '<' else '>'
            else
                for _ in 1 .. abs dx do
                    yield if dx < 0 then '<' else '>'

                for _ in 1 .. abs dy do
                    yield if dy < 0 then '^' else 'v'

            // press the button
            yield 'A'
            // update our position
            x <- nx
            y <- ny
    }

let part1 (data: InputData) =
    // arrowsPad |> Grid.printfn
    // numPad |> Grid.printfn

    data
    |> Seq.map (fun code ->
        let keys = code |> plan numPad false |> plan arrowsPad false |> plan arrowsPad true
        code, keys)
    |> Seq.sumBy (fun (code, keys) -> (keys |> Seq.length) * int (code.TrimEnd('A')))

let part2 (data: InputData) =
    //
    0

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 126384
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 0 // < 161468

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 0
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 0
