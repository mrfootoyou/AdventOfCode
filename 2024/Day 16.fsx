// https://adventofcode.com/2024/day/16
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers
open Direction

[<Literal>]
let Wall = '#'

[<Literal>]
let Open = '.'

[<Literal>]
let Start = 'S'

[<Literal>]
let Exit = 'E'

type InputData = Grid<char> * Coordinates * Coordinates

let parseInput (text: string) : InputData =
    let g = text |> String.splitAndTrim "\n" |> Grid.fromLines
    let s = g |> Grid.find Start
    let e = g |> Grid.find Exit
    (g, s, e) //|> dump

let validateAssumptions ((g, start, exit): InputData) =
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

    if g |> Grid.item 1 (h - 2) <> Start then
        failwith "Start is not in lower left corner."

    if g |> Grid.item (w - 2) 1 <> Exit then
        failwith "Exit is not in upper right corner."

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"""

let sample2 =
    parseData
        """
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"""

let data = lazy (getInput () |> parseData)

[<Literal>]
let MoveCost = 1

[<Literal>]
let TurnCost = 1000

let computeScoreGrid start ((ex, ey) as exit) g =
    let sg =
        g
        |> Grid.map (fun _ c ->
            match c with
            | Wall -> -1
            | _ -> Int32.MaxValue)

    let rec loop moves turns =
        match moves, turns with
        | [], [] -> () // done
        | [] as nextMoves, (prevPos, dir, score) :: nextTurns
        | (prevPos, dir, score) :: nextMoves, nextTurns ->
            let (x, y) as pos = offset prevPos dir

            let (nextMoves, nextTurns) =
                match sg |> Grid.item x y with
                | prevScore when prevScore <= score ->
                    // keep this score (may be a wall)
                    nextMoves, nextTurns
                | prevScore ->
                    // found a new minimum score
                    if pos = start then
                        // final position
                        // turn if needed...
                        let finalScore =
                            match dir with
                            | Right -> score
                            | (Up | Down) -> score + TurnCost
                            | Left -> score + TurnCost + TurnCost

                        if finalScore < prevScore then
                            sg |> Grid.set x y finalScore

                        // continue to explore other options
                        nextMoves, nextTurns
                    else
                        sg |> Grid.set x y score

                        let nextMoves = (pos, dir, score + MoveCost) :: nextMoves

                        let nextTurns =
                            (pos, dir |> turnLeft, score + TurnCost + MoveCost)
                            :: (pos, dir |> turnRight, score + TurnCost + MoveCost) :: nextTurns

                        nextMoves, nextTurns

            loop nextMoves nextTurns

    // give exit a score of 0
    sg |> Grid.set ex ey 0
    // try each direction (no turning penalty)...
    loop
        [ (exit, Left, MoveCost)
          (exit, Right, MoveCost)
          (exit, Up, MoveCost)
          (exit, Down, MoveCost) ]
        []

    sg

let printScoreGrid sg =
    sg
    |> Grid.stringizeFmt
        (function
        | -1 -> " #####"
        | Int32.MaxValue -> "  --  "
        | n -> sprintf "%6d" n)
        " "
    |> printfn "%s"

let part1 ((g, ((sx, sy) as start), exit): InputData) =
    let sg = g |> computeScoreGrid start exit
    // sg |> printScoreGrid
    sg |> Grid.item sx sy

let part2 ((g, ((sx, sy) as start), exit): InputData) =
    let sg = g |> computeScoreGrid start exit
    // sg |> printScoreGrid

    let nextPosVal pos dir =
        let (nx, ny) as np = offset pos dir
        np, sg |> Grid.item nx ny

    let nextVal pos dir =
        let (nx, ny) = offset pos dir
        sg |> Grid.item nx ny

    // NOTE: This algorithm only works when the number of steps is less than TurnCost.
    let nextPos cur pos dir =
        [ // check forward
          let (np, n) = nextPosVal pos dir
          let stepSize = cur - n

          if stepSize = MoveCost then
              // next is a forward move
              yield (np, dir)
          elif (stepSize % TurnCost) = MoveCost then
              // next is a move and turn. check which way(s)...
              if (n - nextVal np (turnRight dir)) % TurnCost = MoveCost then
                  yield (np, turnRight dir)

              if (n - nextVal np (turnLeft dir)) % TurnCost = MoveCost then
                  yield (np, turnLeft dir)

              // could also be a merge point. Check 2 moves forward...
              match nextPosVal np dir with
              | nnp, nn when (cur - nn) % TurnCost = 2 * MoveCost -> yield (nnp, dir)
              | _ -> () ]

    let rec loop tiles next =
        match next with
        | [] -> tiles // done
        | ((x, y) as pos, dir) :: tail ->
            let (tiles, next) =
                match sg |> Grid.item x y with //|> dumps $"c %A{pos}" with
                | -1 -> // wall
                    tiles, tail
                | 0 -> // reached exit
                    tiles |> Set.add pos, tail
                | cur -> tiles |> Set.add pos, (tail @ nextPos cur pos dir)

            loop tiles next

    let tiles =
        [ (start, Right)
          // check if we should also include the tile above Start
          // (the turn without a move confuses our algorithm)...
          match sg |> Grid.item sx sy, nextPosVal start Up with
          | c, (np, n) when (c - n) % TurnCost = MoveCost -> (np, Up)
          | _ -> () ]
        |> loop (Set.empty |> Set.add start)

    // tiles |> Set.iter (fun (x, y) -> sg |> Grid.set x y -2)
    // sg
    // |> Grid.stringizeFmt
    //     (function
    //     | -1 -> "###"
    //     | -2 -> " O "
    //     | Int32.MaxValue -> " - "
    //     | n -> sprintf "%03d" (n % TurnCost))
    //     " "
    // |> printfn "%s"

    tiles.Count

executePuzzle "Part 1 sample1" (fun () -> part1 sample1) 7036
executePuzzle "Part 1 sample2" (fun () -> part1 sample2) 11048
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 135536

executePuzzle "Part 2 sample1" (fun () -> part2 sample1) 45
executePuzzle "Part 2 sample2" (fun () -> part2 sample2) 64
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 583
