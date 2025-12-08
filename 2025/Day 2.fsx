// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers

type InputData = (uint64 * uint64)[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim ","
    |> Seq.map (String.split "-")
    |> Seq.map (fun arr -> UInt64.Parse(arr[0]), UInt64.Parse(arr[1]))
    |> Seq.toArray
// |> echo

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if data |> Array.exists (fun (a, b) -> not (a < b)) then
        failwith "Bad assumption: tuples are not ordered"

    if data |> Array.exists (fun (_, b) -> not (b < fastPow10 11)) then
        failwith "Bad assumption: all values are not less than 10^11"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
"""

let sample2 = sample1

let data = getInput () |> parseData

let mostSignificantPart x lenX n =
    let m = fastPow10 (lenX - n)
    x / m, x % m // msd, remainder

let checkN numGroups (a: uint64) digitCount =
    if digitCount % numGroups <> 0 then
        // skip to next possible match with groupSize digits.
        // For example, from a 4 or 5 digit number we want to jump to
        // 100100100 or 10^8 + 10^5 + 10^2
        let groupSize = (digitCount + numGroups - 1) / numGroups
        let d = fastPow10 (groupSize - 1)

        let mutable next = 0UL

        for _ = 0 to numGroups - 1 do
            next <- next * d * 10UL + d

        false, next
    else
        let groupSize = digitCount / numGroups
        let mutable msv, rem = mostSignificantPart a digitCount groupSize
        let mutable digitCountRem = digitCount - groupSize
        let mutable allMatch = true
        let mutable diff = msv

        while digitCountRem > 0 && allMatch do
            let d, r = mostSignificantPart rem digitCountRem groupSize
            diff <- d
            rem <- r
            digitCountRem <- digitCountRem - groupSize
            allMatch <- diff = msv

        if allMatch && digitCount = 1 then
            // special case for single digit numbers
            allMatch <- false

        let m = fastPow10 groupSize

        let d, groupSize =
            match msv with
            | _ when diff >= msv && msv + 1UL = m -> m, groupSize + 1
            | _ when diff >= msv -> msv + 1UL, groupSize
            | _ -> msv, groupSize

        let mutable next = 0UL

        if numGroups * groupSize > 18 then
            next <- UInt64.MaxValue
        else
            for _ = 0 to numGroups - 1 do
                next <- next * m + d

        allMatch, next

let part1 (data: InputData) =
    let rec loop1 a b sum =
        if a > b then
            sum // done
        else
            let digitCount = countDigitsU64 a
            let isMatch2, next2 = checkN 2 a digitCount

            if isMatch2 then
                // found a match, add to sum
                loop1 next2 b (sum + a)
            else
                // no match, just continue
                loop1 next2 b sum

    data |> Array.Parallel.sumBy (fun (a, b) -> loop1 a b 0UL)

let part2 (data: InputData) =
    let rec loop2 a b sum =
        if a > b then
            sum // done
        else
            let digitCount = countDigitsU64 a
            let isMatch2, next2 = checkN 2 a digitCount
            let isMatch3, next3 = checkN 3 a digitCount
            let isMatch5, next5 = checkN 5 a digitCount
            let isMatchN, nextN = checkN digitCount a digitCount

            let isMatch = isMatch2 || isMatch3 || isMatch5 || isMatchN
            let next = min next2 (min next3 (min next5 nextN))
            if isMatch then loop2 next b (sum + a) else loop2 next b sum

    data |> Array.Parallel.sumBy (fun (a, b) -> loop2 a b 0UL)

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 1227775554UL
executePuzzle "Part 1 finale" (fun () -> part1 data) 18595663903UL

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 4174379265UL
executePuzzle "Part 2 finale" (fun () -> part2 data) 19058204438UL
