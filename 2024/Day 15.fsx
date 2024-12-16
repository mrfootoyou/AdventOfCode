// https://adventofcode.com/2024/day/15
#if INTERACTIVE
#load "../FSharpHelpers.fsx"
#endif

open System
open System.Text.RegularExpressions
open FSharpHelpers

type InputData = Grid<char> * string

let parseInput (text: string) : InputData =
    let lines = text.Trim() |> String.splitRE (Regex @"\r?\n")
    let g = lines |> Array.takeWhile (not << String.IsNullOrEmpty)
    let inst = lines |> Array.skip (g.Length + 1)
    (g |> Grid.fromLines, String.Join("", inst)) //|> dump

let validateAssumptions (data: InputData) =
    // Note: `assert` does not work in FSI, so must throw exception
    if false then
        failwith "bad assumption"

let parseData s = parseInput s |> tee validateAssumptions

let sample1 =
    parseData
        """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""

let sample2 =
    parseData
        """
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"""

let data = lazy (getInput () |> parseData)

[<Literal>]
let Robot = '@'

[<Literal>]
let Box = 'O'

[<Literal>]
let BoxL = '['

[<Literal>]
let BoxR = ']'

[<Literal>]
let Wall = '#'

[<Literal>]
let Open = '.'

[<Literal>]
let Right = '>'

[<Literal>]
let Left = '<'

[<Literal>]
let Up = '^'

[<Literal>]
let Down = 'v'

let delta =
    function
    | Right -> 1, 0
    | Left -> -1, 0
    | Up -> 0, -1
    | Down -> 0, 1
    | _ -> failwith "Unexpected delta"

let (|Delta|) = delta

let answer grid =
    grid
    |> Grid.fold
        (fun s (x, y) ->
            function
            | Box
            | BoxL -> s + (y * 100 + x)
            | _ -> s)
        0

let part1 ((grid, inst): InputData) =
    let grid = grid |> Grid.clone

    let tryMove (x, y) (Delta(dx, dy)) =
        let rec loop x y =
            let (nx, ny) = x + dx, y + dy

            let move () =
                let c = grid |> Grid.item x y
                grid |> Grid.set x y Open
                grid |> Grid.set nx ny c
                true

            match grid |> Grid.item nx ny with
            | Wall -> false
            | Open -> move ()
            | Box -> if loop nx ny then move () else false
            | c -> failwithf "Unexpected sprite: %A" c

        match loop x y with
        | true -> (x + dx, y + dy)
        | _ -> (x, y)

    // g |> Grid.printfn
    inst |> Seq.fold tryMove (grid |> Grid.find Robot) |> ignore
    // g |> Grid.printfn
    grid |> answer

let part2 ((grid, inst): InputData) =
    // double the grid width...
    let grid =
        let g = Grid.create ((grid |> Grid.width) * 2) (grid |> Grid.height) Open

        grid
        |> Grid.iter (fun (x, y) ->
            function
            | Robot ->
                g |> Grid.set (x * 2) y Robot
                g |> Grid.set (x * 2 + 1) y Open
            | Box ->
                g |> Grid.set (x * 2) y BoxL
                g |> Grid.set (x * 2 + 1) y BoxR
            | c ->
                g |> Grid.set (x * 2) y c
                g |> Grid.set (x * 2 + 1) y c)

        g

    let canMove x y dx dy =
        let rec loop x y =
            let (nx, ny) = x + dx, y + dy

            match grid |> Grid.item nx ny with
            | Wall -> false
            | Open -> true
            | (BoxL | BoxR) when dy = 0 ->
                // moving box left or right
                loop nx ny
            | (BoxL | BoxR) as half ->
                // moving box up or down
                // Must be able to move both halves:
                let dh2 = if half = BoxL then +1 else -1
                loop nx ny && loop (nx + dh2) ny
            | c -> failwithf "Unexpected sprite: %A" c

        loop x y

    let tryMove (x, y) (Delta(dx, dy)) =
        if not (canMove x y dx dy) then
            // printfn "Cannot move %A by %A" (x, y) (dx, dy)
            (x, y)
        else
            let move x y =
                let c = grid |> Grid.item x y
                grid |> Grid.set (x + dx) (y + dy) c
                grid |> Grid.set x y Open

            let rec moveNext x y =
                let (nx, ny) = x + dx, y + dy

                match grid |> Grid.item nx ny with
                | Open -> move x y
                | (BoxL | BoxR) when dy = 0 ->
                    // move box left or right
                    moveNext (nx + dx) ny // move front half
                    move nx ny // move back half
                    move x y
                | (BoxL | BoxR) as half when dx = 0 ->
                    // moving box up or down
                    let dh2 = if half = BoxL then +1 else -1
                    moveNext nx ny // move first half
                    moveNext (nx + dh2) ny // move other half
                    move x y
                | c -> failwithf "Unexpected sprite %A at %A" c (nx, ny)

            try
                // printfn "Moving %A by %A" (x, y) (dx, dy)
                moveNext x y
                (x + dx, y + dy)
            with ex ->
                grid |> Grid.printfn
                failwithf "Exception moveNext%A: %s" ((x, y), (dx, dy)) ex.Message

    // grid |> Grid.printfn
    inst |> Seq.fold tryMove (grid |> Grid.find Robot) |> ignore
    // grid |> Grid.printfn
    grid |> answer

executePuzzle "Part 1 sample1" (fun () -> part1 sample1) 2028
executePuzzle "Part 1 sample2" (fun () -> part1 sample2) 10092
executePuzzle "Part 1 finale" (fun () -> part1 data.Value) 1485257

executePuzzle "Part 2 sample" (fun () -> part2 sample2) 9021
executePuzzle "Part 2 finale" (fun () -> part2 data.Value) 1475512
