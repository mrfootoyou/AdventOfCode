// https://adventofcode.com/2024/day/17
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type InputData = int * int * int * byte[]

let parseInput (text: string) : InputData =
    let lines = text |> String.splitAndTrim "\n"

    (lines[ 0 ].Split(": ", 2)[1] |> Int32.Parse,
     lines[ 1 ].Split(": ", 2)[1] |> Int32.Parse,
     lines[ 2 ].Split(": ", 2)[1] |> Int32.Parse,
     lines[ 3 ].Split(": ", 2)[1] |> String.split "," |> Array.map Byte.Parse)
// |> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""

let sample2 =
    parseData
        """
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
"""

let data = lazy (getInput () |> parseData)

[<Literal>]
let Adv = 0uy

[<Literal>]
let Bxl = 1uy

[<Literal>]
let Bst = 2uy

[<Literal>]
let Jnz = 3uy

[<Literal>]
let Bxc = 4uy

[<Literal>]
let Out = 5uy

[<Literal>]
let Bdv = 6uy

[<Literal>]
let Cdv = 7uy

let (|LiteralOp|) (op: byte) = int64 op

let (|ComboOp|) (a: int64) (b: int64) (c: int64) (op: byte) : int64 =
    match op with
    | 0uy
    | 1uy
    | 2uy
    | 3uy as LiteralOp op -> op
    | 4uy -> a
    | 5uy -> b
    | 6uy -> c
    | op -> invalidArg (nameof op) $"Invalid combo operand: {op}"

let printProgram (prog: byte[]) =
    let (|LiteralOp|) (op: byte) = string op

    let (|ComboOp|) op =
        match op with
        | 0uy -> "0"
        | 1uy -> "1"
        | 2uy -> "2"
        | 3uy -> "3"
        | 4uy -> "A"
        | 5uy -> "B"
        | 6uy -> "C"
        | op -> invalidArg (nameof op) $"Invalid combo operand: {op}"

    let rec loop ip =
        if ip >= prog.Length then
            // done. did all outputs match?
            ()
        else
            match prog[ip], prog[ip + 1] with
            | Adv, ComboOp op ->
                printfn "A = A >> %s" op
                loop (ip + 2)
            | Bdv, ComboOp op ->
                printfn "B = A >> %s" op
                loop (ip + 2)
            | Cdv, ComboOp op ->
                printfn "C = A >> %s" op
                loop (ip + 2)
            | Bxl, LiteralOp op ->
                printfn "B = B xor %s" op
                loop (ip + 2)
            | Bst, ComboOp op ->
                printfn "B = %s %% 8" op
                loop (ip + 2)
            | Bxc, _ ->
                printfn "B = B xor C"
                loop (ip + 2)
            | Jnz, LiteralOp op ->
                printfn "if A > 0 then goto %s" op
                loop (ip + 2)
            | Out, ComboOp op ->
                printfn "print (%s %% 8)" op
                loop (ip + 2)
            | _ -> failwith "unknown instruction"

    loop 0
    printfn "ret"

let compute a b c (prog: byte[]) =
    let rec loop a b c ip (out: ResizeArray<byte>) =
        if ip >= prog.Length then
            // done
            out.ToArray()
        else
            match prog[ip], prog[ip + 1] with
            | Adv, ComboOp a b c op -> loop (a >>> int op) b c (ip + 2) out
            | Bdv, ComboOp a b c op -> loop a (a >>> int op) c (ip + 2) out
            | Cdv, ComboOp a b c op -> loop a b (a >>> int op) (ip + 2) out
            | Bxl, LiteralOp op -> loop a (b ^^^ op) c (ip + 2) out
            | Bst, ComboOp a b c op -> loop a (op % 8L) c (ip + 2) out
            | Bxc, _ -> loop a (b ^^^ c) c (ip + 2) out
            | Jnz, LiteralOp op ->
                match a with
                | 0L -> loop a b c (ip + 2) out // no-op
                | _ -> loop a b c (int op) out
            | Out, ComboOp a b c op ->
                out.Add(byte (op % 8L))
                loop a b c (ip + 2) out
            | _ -> failwith "unknown instruction"

    loop a b c 0 (ResizeArray(16))

let part1 ((a, b, c, prog): InputData) =
    compute a b c prog |> Seq.map string |> String.join ","

#nowarn "57" // Experimental library feature (Array.Parallel), requires '--langversion:preview'.

let part2 ((_, b, c, prog): InputData) =
    printProgram prog
    // ---------------
    // B = A % 8
    // B = B xor 5
    // C = A >> B
    // B = B xor 6
    // A = A >> 3
    // B = B xor C
    // print (B % 8)
    // if A > 0 then goto 0
    // ret
    // ---------------

    // Analysis:
    // n = A % 8  // [0..7]
    // B = (n xor 5 xor 6) xor (A >> (n xor 5))
    //   ~> [0..7] xor (A >> [0..7])
    //   ~> 0b00000000_00000??? xor 0b000000??_????????
    // 10-bits (max) determine each output and each value
    // overlaps the previous by 7 bits.
    // Example output: 2,4,1,5,7,5,1,6,0,3,4,0,5,5,3,0
    // ip  a
    // --- -------------------------------------
    // 0   0b•••••••_••••••••_••••••22_22222222
    // 1   0b•••••••_••••••••_•••444^^_^^^^^•••
    // 2   0b•••••••_••••••••_111^^^^^_^^••••••
    // 3   0b•••••••_•••••555_^^^^^^^•_••••••••
    // 4   0b•••••••_••777^^^_^^^^••••_••••••••
    // 5   0b••••••5_55^^^^^^_^•••••••_••••••••
    // 6   0b•••111^_^^^^^^••_••••••••_••••••••
    // 7   0b666^^^^_^^^•••••_••••••••_••••••••
    // ...
    // ||  0b6661115_55777555_11144422_22222222
    let a =
        // Map all 1024 (10-bit) possible inputs to an output value...
        let outputMap =
            Seq.init (1 <<< 10) int64
            |> Seq.fold
                (fun m a ->
                    let res = compute a b c prog
                    let n = res[0]

                    m
                    |> Map.change n (Option.defaultValue Array.empty >> Array.insertAt 0 a >> Some))
                Map.empty // |> echo

        // now find all possible combinations of inputs which produce the
        // correct output AND each input has the same 7 bits as
        // the previous input...
        let rec loop ip candidates =
            if ip = prog.Length then
                candidates
            else
                let n = prog[ip]
                let bs = outputMap |> Map.find n

                if ip = 0 then
                    bs |> Array.toList
                else
                    // return all candidates where A and B have the same 7 bits...
                    let m7 = 0b1111111L <<< (ip * 3)

                    [ for a in candidates do
                          yield!
                              bs
                              |> Seq.choose (fun b ->
                                  match b <<< (3 * ip) with
                                  | b when (a &&& m7) = (b &&& m7) -> Some(a ||| b)
                                  | _ -> None) ]
                |> loop (ip + 1)

        let candidates = loop 0 []

        // return the smallest value.
        candidates |> List.min //|> dumps "a"

    // validate solution is correct...
    if compute a b c prog <> prog then
        failwithf "%d is not a solution!" a

executePuzzle "Part 1 sample" (fun () -> part1 sample1) "4,6,3,5,6,3,5,2,1,0"
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) "7,3,1,3,6,3,6,0,2"

executePuzzle "Part 2 finale" (fun () -> part2 data.Value) ()
