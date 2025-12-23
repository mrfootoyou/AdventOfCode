// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = (string * int[][] * int[])[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        let arr = s.Split(' ')

        arr[0].Trim('[', ']'),
        arr[1 .. arr.Length - 2]
        |> Array.map (fun numStr -> numStr.Trim('(', ')').Split(',') |> Array.map Int32.Parse),
        arr[arr.Length - 1].Trim('{', '}').Split(',') |> Array.map Int32.Parse)
// |> echo

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    for (lights, buttons, joltage) in data do
        affirm (4 <= lights.Length && lights.Length <= 10) "between 4 and 10 lights"

        affirm (joltage.Length = lights.Length) "joltage values pair with lights"
        affirm (Array.TrueForAll(joltage, fun j -> 0 <= j && j <= 999)) "joltage values between 0 and 999"

        affirm (2 <= buttons.Length && buttons.Length <= 13) "between 2 and 13 buttons"

        buttons
        |> Array.iter (fun lightsToggled ->
            affirm (lightsToggled.Length > 0) "one or more lights per button"
            affirm (lightsToggled.Length <= lights.Length) "no more toggles than lights"

            affirm
                (Array.TrueForAll(lightsToggled, fun b -> 0 <= b && b < lights.Length))
                "button values correspond to light indices")

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

let sample2 = sample1

let data = getInput () |> parseData

let part1 (data: InputData) =
    // [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    //           0123
    //          [.##.]  6
    // (3)   => [...#]  8
    // (1,3) => [.#.#] 10
    // (2)   => [..#.]  4
    // (2,3) => [..##] 12
    // (0,2) => [#.#.]  5
    // (0,1) => [##..]  3

    data
    |> Array.sumBy (fun (lights, buttons, _) ->
        let len = lights.Length
        let buttons = buttons |> Array.map (Array.fold (fun acc b -> acc ||| (1 <<< b)) 0)

        let lights =
            lights
            |> Seq.rev
            |> Seq.fold (fun acc c -> acc <<< 1 ||| if c = '#' then 1 else 0) 0

        // let fmt = $"{{0:b0{len}}}"
        // printfn
        //     "lights: %A, buttons: %A"
        //     (String.Format(fmt, lights))
        //     (buttons |> Array.map (fun b -> String.Format(fmt, b)))

        let mutable cont = true
        let mutable i = 1

        while cont && i <= buttons.Length do
            match
                buttons
                |> Seq.allSubsetsOfLength i
                |> Seq.tryFind (fun bs -> bs |> Seq.fold (fun acc b -> acc ^^^ b) 0 = lights)
            with
            | Some _ -> cont <- false
            | None -> i <- i + 1

        if cont then
            failwith "No solution found"

        i)

let part2 (data: InputData) = 0L

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 7
executePuzzle "Part 1 finale" (fun () -> part1 data) 514

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 0L
executePuzzle "Part 2 finale" (fun () -> part2 data) 0L
