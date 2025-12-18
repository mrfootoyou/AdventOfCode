// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Collections.Generic

type InputData = Point3D[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        s.Split(',')
        |> fun arr ->
            { x = Int32.Parse arr[0]
              y = Int32.Parse arr[1]
              z = Int32.Parse arr[2] })
//|> echo

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    data
    |> Array.iter (fun (Coords3D(x, y, z) as pt) ->
        affirm (x >= 0 && y >= 0 && z >= 0) "values are non-negative"
        affirm (squareDistance3D Point3D.zero pt < Int64.MaxValue) "square distance of fits Int64")
    // affirm (Grid.width data = Grid.height data) "grid is square"
    ()

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""

let sample2 = sample1

let data = getInput () |> parseData

let sortByDistance (points: InputData) =
    let list = ResizeArray()

    for i = 0 to points.Length - 2 do
        for j = i + 1 to points.Length - 1 do
            list.Add((i, j, squareDistance3D points[i] points[j]))

    let array = list.ToArray()
    array |> Array.sortInPlaceBy (fun (_, _, dist) -> dist)
    array

let part1 (points: InputData) maxPairs =
    // printfn "Sorting %d points by min distance..." points.Length
    let sortedPoints = sortByDistance points

    // printfn "Assigning each point its own circuit..."
    let pointCircuits = Dictionary<int, int>() // key: point index, value: circuit id
    let circuits = Dictionary<int, int list>() // key: circuit id, value: list of point indexes

    points
    |> Array.iteri (fun i _ ->
        let circuitId = circuits.Count + 1
        circuits[circuitId] <- [ i ]
        pointCircuits[i] <- circuitId)

    // printfn "Calculating circuits..."
    sortedPoints
    |> Seq.take maxPairs
    |> Seq.iter (fun (i, j, dist) ->
        // printfn "Next closest pair: %O and %O with distance %d" points[i] points[j] dist
        match pointCircuits.TryGetValue i, pointCircuits.TryGetValue j with
        | (true, circuitI), (true, circuitJ) when circuitI = circuitJ ->
            // printfn "Points %O and %O already in the same circuit %d" points[i] points[j] circuitI
            ()
        | (true, circuitI), (true, circuitJ) ->
            // Merge circuits
            circuits[circuitJ] |> List.iter (fun j -> pointCircuits[j] <- circuitI)
            circuits[circuitI] <- circuits[circuitJ] @ circuits[circuitI]
            circuits.Remove circuitJ |> ignore
        // printfn "Circuit %d merged into circuit %d" circuitJ circuitI
        | _ -> failwithf "Found unassigned point!")

    // circuits.Count |> traces "Number of circuits"

    circuits.Values
    |> Seq.map List.length
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun acc len -> acc * len) 1

let part2 (points: InputData) =
    // printfn "Sorting %d points by min distance..." points.Length
    let sortedPoints = sortByDistance points

    // printfn "Assigning each point its own circuit..."
    let pointCircuits = Dictionary<int, int>() // key: point index, value: circuit id
    let circuits = Dictionary<int, int list>() // key: circuit id, value: list of point indexes

    points
    |> Array.iteri (fun i _ ->
        let circuitId = circuits.Count + 1
        circuits[circuitId] <- [ i ]
        pointCircuits[i] <- circuitId)

    let rec loop idx =
        let i, j, _ = sortedPoints[idx]

        match pointCircuits.TryGetValue i, pointCircuits.TryGetValue j with
        | (true, circuitI), (true, circuitJ) when circuitI = circuitJ -> loop (idx + 1)
        | (true, circuitI), (true, circuitJ) ->
            // Merge circuits
            circuits[circuitJ] |> List.iter (fun j -> pointCircuits[j] <- circuitI)
            circuits[circuitI] <- circuits[circuitJ] @ circuits[circuitI]
            circuits.Remove circuitJ |> ignore

            if circuits[circuitI] |> List.length = points.Length then
                // printfn "All points connected at points %O and %O" points[i] points[j]
                points[i], points[j]
            else
                loop (idx + 1)
        | _ -> failwithf "Found unassigned point!"

    let a, b = loop 0
    int64 a.x * int64 b.x

executePuzzle "Part 1 sample" (fun () -> part1 sample1 10) 40
executePuzzle "Part 1 finale" (fun () -> part1 data 1000) 97384

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 25272L
executePuzzle "Part 2 finale" (fun () -> part2 data) 9003685096L
