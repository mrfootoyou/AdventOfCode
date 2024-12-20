// https://adventofcode.com/2024/day/19
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type InputData = string[] * string[]

let parseInput (text: string) : InputData =
    let lines = text |> String.splitAndTrim "\n"
    let available = lines[0] |> String.splitAndTrim ","
    let desired = lines |> Array.skip 1
    available, desired

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"""

let sample2 = sample1

let data = lazy (getInput () |> parseData)

/// For performance reasons, `available` should have no "redundant" strings,
/// where a redundant string is one composed of other strings in `available`.
/// Use `removeRedundant` to remove redundant strings.
let findMatch (available: string[]) (s: string) =
    let rec loop si ai matches =
        if si = s.Length then
            matches // matched all
        elif ai = available.Length then
            List.empty // no match
        elif s.AsSpan(si).StartsWith(available[ai]) then
            match loop (si + available[ai].Length) 0 (available[ai] :: matches) with
            | [] -> loop si (ai + 1) matches // continue with next available
            | matches -> matches // matched all
        else
            // continue with next available
            loop si (ai + 1) matches

    loop 0 0 [] |> List.rev |> List.toArray

let isMatch available s = findMatch available s <> Array.empty

/// Removes "redundant" strings from the source array, where a redundant string
/// is one composed of other strings in the source array.
let removeRedundant (available: string[]) =
    available
    |> Array.sortBy (_.Length)
    |> Array.fold
        (fun required a ->
            match isMatch required a with
            | true ->
                // a |> dumps "redundant" |> ignore
                required
            | _ ->
                // a |> dumps "required" |> ignore
                required |> Array.insertAt required.Length a)
        Array.empty

let part1 ((available, desired): InputData) =
    // Required Optimization: Remove available strings which are
    // composed of other available strings. Without this optimization
    // the algorithm will take a long time to run.
    let minAvailable = available |> removeRedundant
    desired |> Array.filter (isMatch minAvailable) |> Seq.length

let part2 ((available, desired): InputData) =
    // let dumps _ x = x
    // let dump x = x
    // let traces _ _ = ()
    // let trace _ = ()

    available |> trace
    let longestAvail = available |> Array.maxBy (_.Length) |> _.Length
    let availableHash = available |> Set.ofArray
    let minAvailable = available |> removeRedundant

    desired
    |> Seq.choose (fun s ->
        match findMatch minAvailable s with
        | [||] -> None
        | m ->
            let mutable subArrangementCache =
                Map.empty
                // pre-load termination conditions
                |> Map.add (m.Length - 1) 1L
                |> Map.add m.Length 1L

            let rec subArrangementCount i =
                match subArrangementCache |> Map.tryFind i with
                | Some arrangements -> arrangements
                | None ->
                    // how many sub-arrangements are there?
                    let mutable arrangements = subArrangementCount (i + 1)

                    // for each pair, triplet, etc, of matches in the hash, add
                    // the following sub-arrangements...
                    let mutable pair = m[i]
                    let mutable j = i + 1

                    while j < m.Length && pair.Length < longestAvail do
                        pair <- pair + m[j]

                        if availableHash |> Set.contains pair then
                            arrangements <- arrangements + subArrangementCount (j + 1)

                        j <- j + 1

                    subArrangementCache <- subArrangementCache |> Map.add i arrangements
                    arrangements //|> dumps $"{i}"

            m |> traces s
            let arrangements = subArrangementCount 0 |> dumps s
            Some arrangements)

    |> Seq.sum

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 6
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 313

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 16L
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 0L // > 41167398997777L
