// https://adventofcode.com/2024/day/20
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers
open Direction
open System.Collections.Generic

[<Literal>]
let Wall = '#'

[<Literal>]
let Open = '.'

[<Literal>]
let Start = 'S'

[<Literal>]
let Exit = 'E'

type InputData = Grid<char>

let parseInput (text: string) : InputData =
    text |> String.splitAndTrim "\n" |> Grid.fromLines //|> dump

let validateAssumptions (g: InputData) =
    let (w, h) = g |> Grid.widthAndHeight

    if
        [ g |> Grid.row 0
          g |> Grid.row (h - 1)
          g |> Grid.col 0
          g |> Grid.col (w - 1) ]
        |> Seq.collect id
        |> Seq.exists ((<>) Wall)
    then
        failwith "Grid edge is not all wall."

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

let findShortestPath grid =
    let start = grid |> Grid.find Start
    let exit = grid |> Grid.find Exit

    let rec loop visited next =
        match next with
        | [] -> None
        | ((x, y) as pos) :: next ->
            let path = visited |> Map.find pos

            if pos = exit then
                Some(pos :: path |> List.rev |> List.toArray)
            else
                let neighbors =
                    [ (x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y) ]
                    |> List.where (fun ((nx, ny) as npos) ->
                        grid |> Grid.item nx ny <> Wall && not (visited |> Map.containsKey npos))

                let nextPath = pos :: path

                let visited =
                    neighbors |> List.fold (fun v npos -> v |> Map.add npos nextPath) visited

                loop visited (neighbors @ next)

    let visited = Map.empty |> Map.add start []

    match loop visited [ start ] with
    | Some path -> path
    | None -> failwith "No path found."

/// Generates all points on the circumference of a "manhattan circle" with
/// center at `(x, y)` and "radius" (manhattan distance) of `dist`.
let manhattanCircle (x, y) dist =
    seq {
        match dist with
        | 0 -> yield (x, y)
        | _ ->
            // corners
            yield (x, y + dist)
            yield (x, y - dist)
            yield (x + dist, y)
            yield (x - dist, y)
            // edges
            for dx = 1 to dist - 1 do
                let dy = dist - dx
                yield (x + dx, y + dy)
                yield (x + dx, y - dy)
                yield (x - dx, y + dy)
                yield (x - dx, y - dy)
    }

let solve (grid: InputData) maxCheats minSaving =
    let path = grid |> findShortestPath

    // Map the path indexes into to their own grid.
    // We could use a lookup Map but this is _significantly_ faster.
    let idxGrid = grid |> Grid.map (fun _ _ -> 0)
    path |> Array.iteri (fun idx (x, y) -> idxGrid |> Grid.set x y idx)

    let mutable shortcuts = 0

    for pathIdx = 0 to path.Length - 1 do
        let pathPos = path[pathIdx]

        for shortcutDist = 2 to maxCheats do
            for (x, y) in manhattanCircle pathPos shortcutDist do
                match idxGrid |> Grid.itemOrDefault x y 0 with
                | shortcutIdx when shortcutIdx - pathIdx > shortcutDist ->
                    let savings = shortcutIdx - pathIdx - shortcutDist

                    if savings >= minSaving then
                        shortcuts <- shortcuts + 1
                | _ -> ()

    shortcuts

let part1 (grid: InputData) minSaving = solve grid 2 minSaving

let part2 (grid: InputData) minSaving = solve grid 20 minSaving

executePuzzle "Part 1 sample" (fun () -> part1 sample1 0) 44
executePuzzle "Part 1 finale" (fun () -> part1 data.Value 100) 1311

executePuzzle "Part 2 sample" (fun () -> part2 sample1 50) 285
executePuzzle "Part 2 finale" (fun () -> part2 data.Value 100) 961364
