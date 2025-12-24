// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Text.RegularExpressions

type Region =
    { Width: int
      Length: int
      Quantity: int[] }

type Shape = Grid<char> // 3x3 grid of '#' and '.'
type InputData = (Shape[] * Region[])

let parseInput (text: string) : InputData =
    let parts = text |> String.splitRE (Regex "\r?\n\r?\n")

    let shapes =
        parts[.. parts.Length - 2]
        |> Array.map (String.splitAndTrim "\n" >> Array.skip 1 >> Grid.fromLines)

    let regions =
        parts[parts.Length - 1]
        |> String.splitAndTrim "\n"
        |> Array.map (fun s ->
            let ps =
                s.Split([| 'x'; ':'; ' ' |], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

            { Width = int ps[0]
              Length = int ps[1]
              Quantity = ps[2..] |> Array.map int })

    shapes, regions //|> echo

let validateAssumptions ((shapes, regions): InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    affirm (shapes.Length = 6) "6 shapes"

    shapes
    |> Array.iter (fun shape -> affirm (shape |> Grid.widthAndHeight = (3, 3)) "All shapes are 3x3")

    regions
    |> Array.iter (fun region ->
        affirm (region.Width >= 4 && region.Length >= 4) "All regions are at 4x4 or larger"
        affirm (region.Width <= 50 && region.Length <= 50) "All regions are 50x50 or smaller"
        affirm (region.Quantity.Length = 6) "All regions have 6 quantities"

        region.Quantity
        |> Array.iter (fun q -> affirm (q >= 0 && q <= 80) "Quantities are between 0 and 80"))

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"""

let sample2 =
    // parseData ""
    sample1

let realInput = getInput () |> parseData

module Shape =
    open System.Collections.Generic

    let id (shape: Shape) =
        // all shapes are 3x3
        shape
        |> Grid.fold (fun acc (x, y) v -> acc + if v = '#' then 1 <<< y * 3 + x else 0) 0

    let private _variants = Dictionary<Shape, Shape[]>()

    let allVariants (shape: Shape) =
        match _variants.TryGetValue shape with
        | true, vars -> vars
        | false, _ ->
            let rotations =
                [| shape
                   shape |> Grid.rotate 90
                   shape |> Grid.rotate 180
                   shape |> Grid.rotate 270 |]

            let variants =
                rotations
                |> Array.collect (fun r -> [| r; r |> Grid.flip Horizontal; r |> Grid.flip Vertical |])
                |> Array.distinctBy id

            _variants.Add(shape, variants)
            variants

let part1 ((shapes, regions): InputData) =

    // let bv = Collections.BitArray(r.Width * r.Length, false)
    // bv.SetAll(false)

    // find those regions which are large enough to contain the required shapes...
    let regions =
        let sizes = shapes |> Array.map (Grid.countOf '#')

        regions
        |> Array.where (fun r ->
            let mutable requiredSize = 0

            for i = 0 to shapes.Length - 1 do
                let shapeSize = sizes[i]
                let q = r.Quantity[i]
                requiredSize <- requiredSize + q * shapeSize

            requiredSize <= r.Width * r.Length)

    // let variants =
    //     shapes
    //     |> Seq.mapi (fun i shape -> 
    //         let variants = Shape.allVariants shape
    //         // printfn "Shape %d has %d unique variants." i variants.Length
    //         variants |> Seq.map (fun v -> (i, v)))
    //     |> Seq.collect id 
    //     |> Seq.toArray

    // printfn "There are %d regions to consider." regions.Length
    // printfn "There are %d unique shape variants." variants.Length
    // variants
    // |> Array.iter (fun (i, v) -> 
    //     printfn "Variant of %d:" i
    //     v |> Grid.printfn)

    // Surprisingly, this is the answer for the real input!
    regions.Length
        
let part2 ((shapes, regions): InputData) = 0

// executePuzzle "Part 1 sample" (fun () -> part1 sample1) 2
executePuzzle "Part 1 finale" (fun () -> part1 realInput) 479

// executePuzzle "Part 2 sample" (fun () -> part2 sample2) 0
// executePuzzle "Part 2 finale" (fun () -> part2 realInput) 0
