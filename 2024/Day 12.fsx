// https://adventofcode.com/2024/day/12
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type InputData = Grid<char>

let parseInput (text: string) : InputData =
    text |> String.splitAndTrim "\n" |> Grid.fromLines //|> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
AAAA
BBCD
BBCC
EEEC
"""

let sample2 =
    parseData
        """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""

let sample3 =
    parseData
        """
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
"""

let sample4 =
    parseData
        """
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
"""

let data = lazy (getInput () |> parseData)

let allGroups (data: InputData) =
    let g = data |> Grid.clone

    let mutable m = Map.empty

    g
    |> Grid.toCoordSeq
    |> Seq.iter (fun ((x, y), v) ->
        if v <> '.' then
            let group = m |> Map.count

            g
            |> Grid.floodFn x y (fun pos ->
                m <-
                    m
                    |> Map.change (v, group) (function
                        | None -> Some [ pos ]
                        | Some group -> Some(pos :: group))

                '.')

            ())

    m.Values

let part1 (data: InputData) =
    let groups = allGroups data

    groups
    |> Seq.sumBy (fun group ->
        let group = group |> List.toArray
        let mutable sharedEdges = 0

        for i = 0 to group.Length - 2 do
            let (ix, iy) = group[i]

            for j = i + 1 to group.Length - 1 do
                let (jx, jy) = group[j]

                match ix - jx, iy - jy with
                | 0, 1
                | 0, -1
                | 1, 0
                | -1, 0 -> sharedEdges <- sharedEdges + 1
                | _ -> ()

        group.Length * ((group.Length * 4) - (sharedEdges * 2)))

type Dir =
    | Up
    | Down
    | Right
    | Left

module Dir =
    let delta: Dir -> Grid.Coordinates =
        function
        | Up -> (0, -1)
        | Down -> (0, +1)
        | Right -> (+1, 0)
        | Left -> (-1, 0)

    let turn leftOrRight =
        function
        | Up -> if leftOrRight = Left then Left else Right
        | Down -> if leftOrRight = Left then Right else Left
        | Right -> if leftOrRight = Left then Up else Down
        | Left -> if leftOrRight = Left then Down else Up

let part2 (g: InputData) =
    let groups = allGroups g

    let calcPrice group =
        // pick a random starting point
        let (x, y) = group |> List.head
        let color = g |> Grid.item x y

        let rec moveToCorner (x, y) dir =
            // Moves counterclockwise around the edge to the next corner.
            // X and Y must be on an edge.
            // Assuming `dir` is Up, then we're at one of these right-most positions:
            //  ##### <- Left-turn corner
            //  ##### <- not a corner
            //  ##### <- not a corner when moving Up
            //  ### <- Up is a Right-turn corner
            //  ### <- not a corner
            let (dx, dy) = Dir.delta dir

            if g |> Grid.itemOrDefault (x + dx) (y + dy) '.' <> color then
                // position A - Left-turn corner
                (x, y), dir |> Dir.turn Left
            else
                // we're at position B or C.
                // move forward then test the right side...
                let (x, y) = (x + dx), (y + dy)
                let right = dir |> Dir.turn Right
                let (dx, dy) = Dir.delta right

                if g |> Grid.itemOrDefault (x + dx) (y + dy) '.' = color then
                    // position B - Right-turn corner
                    (x, y), right
                else
                    // position C - keep moving forward
                    moveToCorner (x, y) dir

        /// Finds a right edge.
        let rec moveRight x y =
            if g |> Grid.itemOrDefault (x + 1) y '.' <> color then
                (x, y) // found right edge
            else
                moveRight (x + 1) y

        let countCorners start =
            let rec loop corners (pos, dir) =
                let next = moveToCorner pos dir
                if next = start then corners else loop (corners + 1) next

            loop 1 start

        // find the next upper-right corner...
        let start = moveToCorner (moveRight x y) Up
        let corners = start |> countCorners
        group.Length * corners

    groups |> Seq.sumBy calcPrice

executePuzzle "Part 1 sample1" (fun () -> part1 sample1) 140
executePuzzle "Part 1 sample2" (fun () -> part1 sample2) 1930
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 1477762

executePuzzle "Part 2 sample1" (fun () -> part2 sample1) 80
executePuzzle "Part 2 sample2" (fun () -> part2 sample2) 1206
executePuzzle "Part 2 sample3" (fun () -> part2 sample3) 236
executePuzzle "Part 2 sample4" (fun () -> part2 sample4) 368
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 0 // > 887866
