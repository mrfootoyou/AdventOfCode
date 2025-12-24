// https://adventofcode.com/2024/day/1
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open FSharpHelpers
open System.Collections.Generic

type InputData = (string * string[])[]

let parseInput (text: string) : InputData =
    text
    |> String.splitAndTrim "\n"
    |> Array.map (fun s ->
        let arr = s.Split(':')
        arr[0], arr[1] |> String.splitAndTrim " ")
// |> echo

let validateAssumptions (data: InputData) =
    let affirm condition msg =
        if not condition then
            failwithf "Assumption failed: '%s' is not true." msg

    ()


let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

let sample2 =
    parseData
        """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""

let realInput = getInput () |> parseData

let part1 (data: InputData) =
    let map = data |> Map.ofArray

    let mutable count = 0
    let toVisit = Queue<string> [ "you" ]
    let visited = HashSet<string>()

    while toVisit.Count > 0 do
        let node = toVisit.Dequeue()

        if node = "out" then
            count <- count + 1
        else
            for child in map[node] do
                if not (visited.Contains child) then
                    toVisit.Enqueue child

    count

let part2 (data: InputData) =

    // // Visualize the graph using GraphViz...
    // printfn "digraph g {"
    // printfn "  \"svr\" [fillcolor = green, style=filled]"
    // printfn "  \"out\" [fillcolor = red, style=filled]"
    // printfn "  \"fft\" [fillcolor = blue, style=filled]"
    // printfn "  \"dac\" [fillcolor = blue, style=filled]"
    // for node, children in data do
    //     for child in children do
    //         printfn "  \"%s\" -> \"%s\"" node child
    // printfn "}"

    let map = data |> Map.ofSeq

    let pathCount start target =
        let path = HashSet()
        let cache = Dictionary<string * string, int>()

        let rec dfs node hasDAC hasFFT =
            if node = target then
                // found an exit
                // if hasDAC && hasFFT then 1 else 0
                1
            else
                let mutable count = 0

                if path.Add node then
                    let hasDAC = hasDAC || node = "dac"
                    let hasFFT = hasFFT || node = "fft"
                    // (node, hasDAC, hasFFT) |> trace
                    match map |> Map.tryFind node with
                    | Some children ->
                        for child in children do
                            match cache.TryGetValue((node, child)) with
                            | true, c -> count <- count + c
                            | false, _ ->
                                // printf "."
                                let c = dfs child hasDAC hasFFT
                                // printf "\b \b"
                                cache[(node, child)] <- c
                                count <- count + c
                    | None -> ()

                    path.Remove node |> ignore

                count

        map[start] |> ignore
        dfs start false false

    // pathCount "svr" "out" // returns wrong answer

    // NOTE: Used GraphViz to determine that all paths go through fft then dac
    let svr_fft = pathCount "svr" "fft"
    let fft_dac = pathCount "fft" "dac"
    let dac_out = pathCount "dac" "out"
    int64 svr_fft * int64 fft_dac * int64 dac_out

executePuzzle "Part 1 sample" (fun () -> part1 sample1) 5
executePuzzle "Part 1 finale" (fun () -> part1 realInput) 670

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 2L
executePuzzle "Part 2 finale" (fun () -> part2 realInput) 332052564714990L
