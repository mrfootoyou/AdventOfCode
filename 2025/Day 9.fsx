// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Collections.Generic

type InputData = (int * int)[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s -> s.Split(',') |> fun arr -> Int32.Parse arr[0], Int32.Parse arr[1])
//|> echo

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    data
    |> Array.iter (fun (x, y) -> affirm (x > 0 && y > 0) "all values are positive")

    affirm (data.Length % 2 = 0) "even number of points"

    match data[0], data[1] with
    | (x0, y0), (x1, y1) when x0 = x1 ->
        // Vertical start
        // Verify we alternate vertical/horizontal
        for i = 2 to data.Length - 2 do
            let (xPrev, yPrev) = data[i - 1]
            let (xCurr, yCurr) = data[i]
            let (xNext, yNext) = data[i + 1]

            if xCurr = xPrev then
                affirm
                    (yNext = yCurr)
                    (sprintf
                        "points %A, %A, %A are not alternating vertical/horizontal"
                        (xPrev, yPrev)
                        (xCurr, yCurr)
                        (xNext, yNext))
            else
                affirm
                    (xNext = xCurr)
                    (sprintf
                        "points %A, %A, %A are not alternating vertical/horizontal"
                        (xPrev, yPrev)
                        (xCurr, yCurr)
                        (xNext, yNext))

        ()
    | (x0, y0), (x1, y1) when y0 = y1 ->
        // Horizontal start
        // Verify we alternate horizontal/vertical
        for i = 2 to data.Length - 2 do
            let (xPrev, yPrev) = data[i - 1]
            let (xCurr, yCurr) = data[i]
            let (xNext, yNext) = data[i + 1]

            if yCurr = yPrev then
                affirm
                    (xNext = xCurr)
                    (sprintf
                        "points %A, %A, %A are not alternating horizontal/vertical"
                        (xPrev, yPrev)
                        (xCurr, yCurr)
                        (xNext, yNext))
            else
                affirm
                    (yNext = yCurr)
                    (sprintf
                        "points %A, %A, %A are not alternating horizontal/vertical"
                        (xPrev, yPrev)
                        (xCurr, yCurr)
                        (xNext, yNext))

        ()
    | _ -> failwith "First two points must be aligned either vertically or horizontally"

    ()

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 (data: InputData) =
    seq {
        for i = 0 to data.Length - 2 do
            for j = i + 1 to data.Length - 1 do
                yield i, j
    }
    |> Seq.map (fun (i, j) ->
        let (x1, y1) = data[i]
        let (x2, y2) = data[j]
        let dx = abs (x2 - x1) + 1
        let dy = abs (y2 - y1) + 1
        int64 dx * int64 dy)
    |> Seq.max

let part2 (path: InputData) =
    let minX = path |> Array.map fst |> Array.min
    let maxX = path |> Array.map fst |> Array.max
    let minY = path |> Array.map snd |> Array.min
    let maxY = path |> Array.map snd |> Array.max

    let bounds = Rect.fromCoords (minX, minY) (maxX + 1, maxY + 1) //|> tee (printfn "Bounds: %O")

    // Determine winding direction
    // Clockwise if right turns are 4 more than left turns
    let segments =
        [| for i = 0 to path.Length - 1 do
               let x0, y0 as p0 = path[i]
               let x1, y1 as p1 = path[(i + 1) % path.Length]

               let dir =
                   if x1 = x0 && y1 < y0 then North
                   elif x1 = x0 && y1 > y0 then South
                   elif y1 = y0 && x1 < x0 then West
                   elif y1 = y0 && x1 > x0 then East
                   else failwithf "Unexpected segment: %A -> %A" p0 p1

               yield p0, p1, dir |]
    // |> dumps "Segments"

    let angles =
        [| for i = 0 to segments.Length - 1 do
               let (x0, y0 as p0), (x1, y1 as p1), dir1 = segments[i]
               let _, (x2, y2 as p2), dir2 = segments[(i + 1) % segments.Length]

               let turn =
                   match dir1, dir2 with
                   | North, East
                   | East, South
                   | South, West
                   | West, North -> Right
                   | North, West
                   | West, South
                   | South, East
                   | East, North -> Left
                   | _ -> failwithf "Unexpected angle: %A %A %A %A %A" p0 dir1 p1 dir2 p2

               let len1 = max (abs (x1 - x0)) (abs (y1 - y0)) + 1
               let len2 = max (abs (x2 - x1)) (abs (y2 - y1)) + 1
               yield len1, len2, turn |]
    // |> dumps "Angles"

    let clockwise, insideAngles =
        let leftTurns = angles |> Array.where (fun (_, _, turn) -> turn = Left)
        let rightTurns = angles |> Array.where (fun (_, _, turn) -> turn = Right)

        if rightTurns.Length - leftTurns.Length = 4 then
            true, rightTurns // clockwise (center is to the right of the path)
        else
            false, leftTurns // counter-clockwise

    let grid = Grid.create bounds.width bounds.height '.'

    segments
    |> Array.iter (fun ((x0, y0), (x1, y1), dir) ->
        let xStart = min x0 x1 - bounds.p1.x
        let xEnd = max x0 x1 - bounds.p1.x
        let yStart = min y0 y1 - bounds.p1.y
        let yEnd = max y0 y1 - bounds.p1.y

        for x = xStart to xEnd do
            for y = yStart to yEnd do
                grid |> Grid.set x y '#')

    printfn "Traced path"
    // grid |> Grid.printfn

    let inside compassDir =
        let dir = compassDir |> Direction.fromCompass

        if clockwise then
            Direction.turnRight dir
        else
            Direction.turnLeft dir

    segments
    |> Array.iter (fun ((x0, y0), (x1, y1), dir) ->
        let xStart = min x0 x1 - bounds.p1.x
        let xEnd = max x0 x1 - bounds.p1.x
        let yStart = min y0 y1 - bounds.p1.y
        let yEnd = max y0 y1 - bounds.p1.y

        for x = xStart to xEnd do
            for y = yStart to yEnd do
                let dx, dy = inside dir |> Direction.delta

                if grid |> Grid.item (x + dx) (y + dy) = '.' then
                    grid |> Grid.flood (x + dx) (y + dy) '#')

    printfn "Filled path"
    // grid |> Grid.printfn

    // for each pair of path points, check if the rectangle they define is fully inside
    // the path. Find the largest such rectangle...
    let mutable maxArea = 0L

    for i = 0 to path.Length - 2 do
        let (x0, y0) = path[i]

        for j = i + 1 to path.Length - 1 do
            let (x1, y1) = path[j]

            let xStart = min x0 x1 - bounds.p1.x
            let xEnd = max x0 x1 - bounds.p1.x
            let yStart = min y0 y1 - bounds.p1.y
            let yEnd = max y0 y1 - bounds.p1.y

            // rule out smaller areas right away
            let area = int64 (xEnd - xStart + 1) * int64 (yEnd - yStart + 1)
            let mutable isMatch = area > maxArea

            // quick check opposite corners
            isMatch <-
                isMatch
                && grid |> Grid.item xStart yEnd = '#'
                && grid |> Grid.item xEnd yStart = '#'

            // check top and bottom edges
            let mutable x = xStart + 1

            while isMatch && x < xEnd do
                if grid |> Grid.item x yStart = '#' && grid |> Grid.item x yEnd = '#' then
                    x <- x + 1
                else
                    isMatch <- false

            // check left and right edges
            let mutable y = yStart + 1

            while isMatch && y < yEnd do
                if grid |> Grid.item xStart y = '#' && grid |> Grid.item xEnd y = '#' then
                    y <- y + 1
                else
                    isMatch <- false

            // could be a hole in the middle, but assuming not for now...
            if isMatch then
                maxArea <- area

                printfn
                    "Max area: %d (%0.2f%% complete)"
                    maxArea
                    (float (i * path.Length + j) / float (path.Length * path.Length) * 100.0)

    maxArea

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 50L
executePuzzle "Part 1 finale" (fun () -> part1 data) 4781235324L

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 24L
executePuzzle "Part 2 finale" (fun () -> part2 data) 1566935900L
