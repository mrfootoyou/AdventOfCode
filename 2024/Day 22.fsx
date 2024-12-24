// https://adventofcode.com/2024/day/X
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

#nowarn "57" // Experimental library feature (Array.Parallel), requires '--langversion:preview'.

open System
open System.Text.RegularExpressions
open FSharpHelpers
open System.Collections.Generic
open System.Collections.Concurrent

type InputData = int[]

let parseInput (text: string) : InputData =
    text |> String.splitAndTrim "\n" |> Array.map int //|> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
1
10
100
2024
"""

let sample2 =
    parseData
        """
1
2
3
2024
"""

let data =
    let rawData = getInput ()
    lazy (rawData |> parseData)

let inline mix value secret = value ^^^ secret
let inline prune secret = secret % 16777216L
let inline maxAndPrune secret value = secret |> mix value |> prune

let nextSecret secret =
    // let secret = int (int64 secret * 64L) |> maxAndPrune secret
    // let secret = int (int64 secret / 32L) |> maxAndPrune secret
    // let secret = int (int64 secret * 2048L) |> maxAndPrune secret
    let secret = ((secret <<< 6) ^^^ secret) &&& 0x00ff_ffff (*24 bits*)
    let secret = ((secret >>> 5) ^^^ secret) &&& 0x00ff_ffff
    let secret = ((secret <<< 11) ^^^ secret) &&& 0x00ff_ffff
    // shifts 12 bits each iteration, so 2000 iterations will shift 24000 bits

    secret

let part1 (data: InputData) =
    let rec loop iter secret =
        match iter with
        | 2000 -> secret
        | _ -> nextSecret secret |> loop (iter + 1)

    data |> Array.sumBy (loop 0 >> int64)

type Price = int8 // (0..9)
type DiffPrice = int8 // (-9..18)
type Diff4 = uint // 4 diffs packed into an uint32

let makeDiff (d1: DiffPrice) (d2: DiffPrice) (d3: DiffPrice) (d4: DiffPrice) : Diff4 =
    ((d1 |> sbyte |> byte |> uint) <<< 24)
    ||| ((d2 |> sbyte |> byte |> uint) <<< 16)
    ||| ((d3 |> sbyte |> byte |> uint) <<< 8)
    ||| ((d4 |> sbyte |> byte |> uint) <<< 0)

let unDiff (diff: Diff4) : (DiffPrice * DiffPrice * DiffPrice * DiffPrice) =
    let d1 = diff >>> 24 &&& 0x0ffu |> byte |> sbyte
    let d2 = diff >>> 16 &&& 0x0ffu |> byte |> sbyte
    let d3 = diff >>> 8 &&& 0x0ffu |> byte |> sbyte
    let d4 = diff >>> 0 &&& 0x0ffu |> byte |> sbyte
    (d1, d2, d3, d4)

let nextPrice initialSecret : Price seq =
    let mutable next = initialSecret

    Seq.initInfinite (fun _ ->
        let value = next % 10 |> int8
        next <- nextSecret next
        value)

let diffSeq count initialSecret =
    use e = (nextPrice initialSecret).GetEnumerator()

    let next () =
        match e.MoveNext() with
        | true -> e.Current
        | _ -> failwith "cannot happen"

    let mutable n1 = next ()
    let mutable n2 = next ()
    let mutable n3 = next ()
    let mutable n4 = next ()

    Seq.init count (fun _ ->
        let n5 = next ()
        let diffs = makeDiff (n2 - n1) (n3 - n2) (n4 - n3) (n5 - n4)
        n1 <- n2
        n2 <- n3
        n3 <- n4
        n4 <- n5
        (diffs, n5))

let part2 (data: InputData) =
    // index every diff, recording the first price of each secret...
    let allDiffs =
        ConcurrentDictionary<Diff4, int>(concurrencyLevel = -1, capacity = 2000 * data.Length * 90 / 100)

    data
    |> Array.Parallel.iter (fun secret ->
        let firstDiffPrices = secret |> diffSeq 2000 |> Seq.distinctBy fst

        for (diff, price) in firstDiffPrices do
            allDiffs.AddOrUpdate(diff, (fun _ -> int price), (fun _ total -> total + int price))
            |> ignore)

    // allDiffs.Count |> traces "allDiffs.Count"
    // allDiffs[diff] |> trace

    allDiffs.Values |> Seq.max

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 37327623L
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 18525593556L

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 23
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 0 // < 2096
