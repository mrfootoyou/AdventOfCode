open System
open System.Numerics

/// Executes the given unit function with the input value and returns the input value.
let inline tee ([<InlineIfLambda>] fn) input =
    fn input
    input

/// Prints the given value to stdout, then returns the value.
let inline dump x =
#if INTERACTIVE
    // use sprintf to avoid "tearing" the output
    tee (sprintf "%A" >> Console.WriteLine) x
#else
    LINQPad.Extensions.Dump(x)
#endif

/// Prints the given value to stdout with the given heading prefix, then returns the value.
let inline dumps (heading: string) x =
#if INTERACTIVE
    // use sprintf to avoid "tearing" the output
    tee (sprintf "%s: %A" heading >> Console.WriteLine) x
#else
    LINQPad.Extensions.Dump(heading, x)
#endif

/// Same as `dump`.
let echo = dump
/// Same as `dumps`.
let echos = dumps

/// Same as `dump` but returns no value.
let inline trace x = dump x |> ignore
/// Same as `dumps` but returns no value.
let inline traces s x = dumps s x |> ignore

let scriptPath =
#if INTERACTIVE
    fsi.CommandLineArgs[0]
#else
    LINQPad.Util.CurrentQueryPath
#endif

/// Downloads puzzle input as a string.
/// Assumes script file is named "./{Year}/Day {day}.fsx" and
/// cookie.txt exists in repo root.
let downloadInput () =
    let fi = IO.FileInfo(scriptPath)
    let year = fi.Directory.Name |> int

    let day =
        IO.Path.GetFileNameWithoutExtension(fi.Name).Split(' ') |> Array.last |> int

    let cookiePath = IO.Path.Join(fi.Directory.Parent.FullName, "cookie.txt")
    let cookie = IO.File.ReadAllLines(cookiePath)[0]

    use client = new Net.Http.HttpClient()
    client.DefaultRequestHeaders.Add("cookie", cookie.Trim())

    client.GetStringAsync($"https://adventofcode.com/{year}/day/{day}/input")
    |> Async.AwaitTask
    |> Async.RunSynchronously

/// Returns the path of the file containing puzzle input.
let getInput () =
    let inputFilePath = IO.Path.ChangeExtension(scriptPath, ".txt")

    if not (IO.File.Exists(inputFilePath)) then
        let input = downloadInput ()
        IO.File.WriteAllText(inputFilePath, input)

    IO.File.ReadAllText(inputFilePath)


let test oper operName title expected actual =
    if not (oper expected actual) then
        failwithf "%s should %s [%A] but is [%A]." title operName expected actual

let testEqual<'T> = test (=) "equal"

let testNotEqual<'T> = test (<>) "not equal"

let measure fn =
    let start = Diagnostics.Stopwatch.GetTimestamp()
    let res = fn ()
    res, Diagnostics.Stopwatch.GetElapsedTime(start)

let inline executePuzzle title fn expected =
    let (res, elapsed) = measure fn
    printfn "[%s] %s: %A" (elapsed.ToString("G")) title res
    testEqual title expected res

// Static lookup table for powers of 10 (cache-hot in tight loops)
let powersOf10 =
    [| 1UL
       10UL
       100UL
       1000UL
       10000UL
       100000UL
       1000000UL
       10000000UL
       100000000UL
       1000000000UL // 10^9 (Max for int32/uint32)
       10000000000UL
       100000000000UL
       1000000000000UL
       10000000000000UL
       100000000000000UL
       1000000000000000UL
       10000000000000000UL
       100000000000000000UL
       1000000000000000000UL // 10^18 (Max for int64)
       10000000000000000000UL |] // 10^19 (Max for uint64)

/// Counts the number of decimal digits in an unsigned 64-bit integer.
let countDigitsU64 (n: uint64) =
    if n = 0UL then
        1
    else
        // 1. Hardware Intrinsic (~3 cycles)
        // Get number of bits used. (64 - leading zeros)
        let bits = 64 - System.Numerics.BitOperations.LeadingZeroCount(n)

        // 2. Approximate Log10 using Fixed-Point Math
        // log10(2) is approx 0.30103.
        // 1233 / 4096 (2^12) is 0.301025.
        // This maps the bit-count to a base-10 magnitude.
        let t = (bits * 1233) >>> 12

        // 3. Correction
        // The approximation may be off by one at power-of-10 boundaries.
        // We check against the table to be precise.
        if n < powersOf10[t] then t else t + 1

/// Counts the number of decimal digits in the given signed 64-bit integer.
let countDigitsI64 (n: int64) =
    if n >= 0L then
        countDigitsU64 (uint64 n)
    else
        countDigitsU64 (uint64 -n)

/// Computes 10 raised to the given exponent (0..19) as a uint64.
/// Uses a static lookup table for performance.
/// Throws `ArgumentOutOfRangeException` if exponent is out of range.
/// - exp: The exponent. 0..19 for uint64. 18 is the max for int64 values, 9 for int32/uint32.
let inline fastPow10 (exp: int) =
    if exp < 0 || exp > 19 then
        invalidArg (nameof exp) "Exponent out of range (0..19)."

    powersOf10[exp]

/// Computes the sum of positive integers in the range 0..n, inclusive.
let summatorial n = (n * (n + 1)) / 2

/// Returns `(b, a)` if the `condition` is true, otherwise `(a, b)`.
let inline swapIf condition a b = if condition then (b, a) else (a, b)

/// Returns `(b, a)` if the predicate returns true, otherwise `(a, b)`.
let inline swapWhen ([<InlineIfLambda>] pred) a b = if pred () then (b, a) else (a, b)

/// Converts the given digit character ('0'..'9') to its numeric equivalent (0..9).
let digitToInt (c: char) =
    match int c with
    | n when (int '0') <= n && n <= (int '9') -> n - (int '0')
    | _ -> invalidArg (nameof c) (sprintf "Invalid decimal digit: %A" c)

/// Converts the given hexdigit character ('0'..'F') to its numeric equivalent (0..15).
let hexDigitToInt (c: char) =
    match int c with
    | n when (int '0') <= n && n <= (int '9') -> n - (int '0')
    | n when (int 'A') <= n && n <= (int 'F') -> n - (int 'A') + 10
    | n when (int 'a') <= n && n <= (int 'f') -> n - (int 'a') + 10
    | _ -> invalidArg (nameof c) (sprintf "Invalid hex digit: %A" c)

let (|DecChar|) = digitToInt
let (|HexChar|) = hexDigitToInt

[<TailCall>]
let factorial n =
    let rec fact acc =
        function
        | n when n <= 1 -> acc
        | n -> fact (acc * int64 n) (n - 1)

    assert (n >= 0)
    fact 1L n

type XCoord = int
type YCoord = int
/// The X-Y coordinates of a grid position.
type Coordinate2D = (XCoord * YCoord)
type Coordinates = Coordinate2D

[<Struct>]
type Compass =
    | North
    | South
    | East
    | West

[<Struct>]
type Direction =
    | Up
    | Down
    | Right
    | Left

module Direction =
    let toCompass dir =
        match dir with
        | Up -> North
        | Down -> South
        | Right -> East
        | Left -> West

    let fromCompass cd =
        match cd with
        | North -> Up
        | South -> Down
        | East -> Right
        | West -> Left

    let reverse dir =
        match dir with
        | Up -> Down
        | Down -> Up
        | Right -> Left
        | Left -> Right

    let turn leftOrRight dir =
        match leftOrRight with
        | Left ->
            match dir with
            | Up -> Left
            | Down -> Right
            | Right -> Up
            | Left -> Down
        | Right ->
            match dir with
            | Up -> Right
            | Down -> Left
            | Right -> Down
            | Left -> Up
        | _ -> invalidArg (nameof leftOrRight) ""

    let inline turnLeft dir = turn Left dir
    let inline turnRight dir = turn Right dir

    /// Turn clockwise in 90deg increments.
    /// Degrees can be negative or even greater than 360.
    let rotate degrees dir =
        match (degrees % 360 + 360) % 360 with
        | 0 -> dir
        | 90 -> turnRight dir
        | 180 -> reverse dir
        | 270 -> turnLeft dir
        | _ -> invalidArg (nameof degrees) "Rotation angle must be a multiple of 90 degrees."

    /// Returns a unit vector for the given direction.
    /// Uses "screen" coordinates where Up is (0,-1) and Right is (+1,0).
    let delta dir : Coordinates =
        match dir with
        | Up -> (0, -1)
        | Down -> (0, +1)
        | Right -> (+1, 0)
        | Left -> (-1, 0)

    let inline (|Delta|) dir = delta dir

    let inline offset (x, y) (Delta(dx, dy)) = (x + dx, y + dy)

    let toArrow dir =
        match dir with
        | Up -> '^'
        | Down -> 'v'
        | Right -> '>'
        | Left -> '<'

    let fromArrow char =
        match char with
        | '^' -> Up
        | 'v' -> Down
        | '>' -> Right
        | '<' -> Left
        | _ -> invalidArg (nameof char) "Unexpected arrow character."

[<Struct>]
type Orientation =
    | Horizontal
    | Vertical

module Regex =
    open System.Text.RegularExpressions

    let matchGroups (re: string) (s: string) =
        let m = Regex.Match(s, re)
        if not m.Success then None else Some m.Groups

    let (|GroupNum|_|) (n: int) (gs: GroupCollection) =
        match gs[n] with
        | m when m.Success -> Some m.Value
        | _ -> None

    let (|GroupName|_|) (n: string) (gs: GroupCollection) =
        match gs[n] with
        | m when m.Success -> Some m.Value
        | _ -> None


module String =
    open System.Text.RegularExpressions

    let comparer = StringComparer.Ordinal
    let compareri = StringComparer.OrdinalIgnoreCase

    let inline len (s: string) = s.Length

    let inline isBlank s = String.IsNullOrWhiteSpace s
    let inline isEmpty s = String.IsNullOrEmpty s
    let inline compare a b = comparer.Compare(a, b)
    let inline comparei a b = compareri.Compare(a, b)
    let inline equal a b = compare a b = 0
    let inline equali a b = comparei a b = 0
    let inline startsWith (prefix: string) (s: string) = s.StartsWith(prefix)
    let inline endsWith (suffix: string) (s: string) = s.EndsWith(suffix)
    let inline indexOf (x: string) (s: string) = s.IndexOf(x)
    let inline contains (x: string) (s: string) = s.Contains(x, StringComparison.Ordinal)

    let inline containsi (x: string) (s: string) =
        s.Contains(x, StringComparison.OrdinalIgnoreCase)

    let inline substr idx max (s: string) =
        if 0 <= max && max < s.Length - idx then
            s.Substring(idx, max)
        else
            s.Substring(idx)

    let inline left max (s: string) = substr 0 max s

    let inline right max (s: string) =
        if 0 <= max && max < s.Length then
            s.Substring(s.Length - max)
        else
            s

    let inline toArray (s: string) = s.ToCharArray()
    let inline toSeq (s: string) : char seq = s :> _
    let inline ofArray (cs: char[]) = String.Concat(cs)
    let inline ofSeq (cs: char seq) = String.Concat(cs)

    let inline toUpper (s: string) = s.ToUpperInvariant()
    let inline toLower (s: string) = s.ToLowerInvariant()

    let inline trim (s: string) = s.Trim()
    let inline trimL (s: string) = s.TrimStart()
    let inline trimR (s: string) = s.TrimEnd()

    let inline padL c totalWidth (s: string) = s.PadLeft(c, totalWidth)
    let inline padR c totalWidth (s: string) = s.PadRight(c, totalWidth)

    let inline join (delim: string) (ss: string seq) = String.Join(delim, ss)

    let inline rev (s: string) =
        String.Create(
            s.Length,
            s,
            fun dst s ->
                s.CopyTo(dst)
                MemoryExtensions.Reverse(dst)
        )

    /// Split a string using the given delimiter.
    let inline split (sep: string) (s: string) = s.Split(sep)
    /// Split a string into at most `count` parts.
    let inline splitN (sep: string) count (s: string) = s.Split(sep, count = count)
    /// Split a string using the given `StringSplit` options.
    let inline splitO (sep: string) opts (s: string) = s.Split(sep, options = opts)

    /// Split a string while trimming and removing empty values.
    let inline splitAndTrim (sep: string) (s: string) =
        splitO sep (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) s

    /// Split a string into at most `count` parts using the given `StringSplit` options.
    let inline splitNO (sep: string) count opts (s: string) =
        s.Split(sep, count = count, options = opts)

    /// Split a string using the given regular expression as a delimter.
    let inline splitRE (re: Regex) (s: string) = re.Split(s)

    let inline replace (oldValue: string) newValue (s: string) =
        s.Replace(oldValue, newValue, StringComparison.Ordinal)

    let inline replacei (oldValue: string) newValue (s: string) =
        s.Replace(oldValue, newValue, StringComparison.OrdinalIgnoreCase)

    let inline replaceRE (re: Regex) replacement (s: string) =
        re.Replace(s, replacement = replacement)

    let inline replaceREWith (re: Regex) ([<InlineIfLambda>] evaluator) (s: string) =
        re.Replace(s, MatchEvaluator(evaluator))

    let inline tryMatch (re: Regex) (s: string) =
        match re.Match(s) with
        | m when m.Success -> Some m.Groups
        | _ -> None

module StringBuilder =
    open System.Text

    let create (capacity: int) = StringBuilder(capacity)

    let inline append (x: string) (sb: StringBuilder) = sb.Append(x)

    let inline appendLine (x: string) (sb: StringBuilder) = sb.AppendLine(x)

    let inline appendFmt (fmt: string) (x: string) (sb: StringBuilder) = sb.AppendFormat(fmt, x)

module Int32 =
    let inline toString toBase (num: Int32) = Convert.ToString(num, toBase = toBase)
    let inline fromString fromBase (s: string) = Convert.ToInt32(s, fromBase = fromBase)

    /// Computes the Greatest Common Divisor (GCD) of two integers
    [<TailCall>]
    let gcd a b =
        let rec loop a b =
            if b = 0 then a
            elif a > b then loop b (a % b)
            else loop a (b % a)

        loop (abs a) (abs b)

    /// Computes the Greatest Common Divisor (GCD) of a list of integers
    let gcdList nums =
        match nums with
        | [] -> 0
        | head :: rest -> rest |> List.fold gcd head

    /// Computes the Least Common Multiple (LCM) of two integers
    /// Use the Int64 version if there is any chance of overflow.
    let lcm a b =
        match a, b with
        | 0, _
        | _, 0 -> 0
        | _ -> abs (a * b) / gcd a b

    /// Computes the Least Common Multiple (LCM) of a list of integers
    /// Use the Int64 version if there is any chance of overflow.
    let lcmList nums =
        match nums with
        | [] -> 0
        | head :: rest -> rest |> List.fold lcm head

module Int64 =
    let inline toString toBase (num: Int64) = Convert.ToString(num, toBase = toBase)
    let inline fromString fromBase (s: string) = Convert.ToInt64(s, fromBase = fromBase)

    /// Computes the Greatest Common Divisor (GCD) of two integers
    [<TailCall>]
    let gcd a b =
        let rec loop a b =
            if b = 0L then a
            elif a > b then loop b (a % b)
            else loop a (b % a)

        loop (abs a) (abs b)

    /// Computes the Greatest Common Divisor (GCD) of a list of integers
    let gcdList nums =
        match nums with
        | [] -> 0L
        | head :: rest -> rest |> List.fold gcd head

    /// Computes the Least Common Multiple (LCM) of two integers
    let lcm a b =
        match a, b with
        | 0L, _
        | _, 0L -> 0L
        | _ -> abs (a * b) / gcd a b

    /// Computes the Least Common Multiple (LCM) of a list of integers
    let lcmList nums =
        match nums with
        | [] -> 0L
        | head :: rest -> rest |> List.fold lcm head


module Array =
    /// Splits the array into two parts -- those before index `n`, and those
    /// at or after index `n`.
    /// If `n` is less than or equal to zero, the first part is empty.
    /// If `n` is greater than or equal to the length of the array, the second part is empty.
    let inline splitAtIndex n a =
        match n with
        | _ when n <= 0 -> Array.empty, a |> Array.copy
        | _ when n >= Array.length a -> a |> Array.copy, Array.empty
        | _ -> a[.. (n - 1)], a[n..]

    /// Splits the array into two parts -- those prior to the index where the
    /// predicate first returns `true`, and the others.
    /// If the predicate never returns `true`, the first tuple item is the entire array.
    /// If the predicate returns `true` for the first item, the first tuple item is empty.
    let inline split ([<InlineIfLambda>] pred: 'a -> bool) a =
        a |> splitAtIndex (a |> Array.tryFindIndex pred |> Option.defaultValue a.Length)

    /// Return a new array shuffled in a random order.
    let shuffle = Array.randomShuffle

    /// Swaps two elements in an array in-place.
    let inline swap i j (arr: 'a[]) =
        let tmp = arr[i]
        arr[i] <- arr[j]
        arr[j] <- tmp

    /// Generates all permutations of a subset of an array of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Warning: this function permutes the source array in-place.
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    ///
    /// # Parameters
    /// - start: The starting index of the elements to permute.
    /// - len: The number of elements to permute.
    /// - arr: The source array.
    let permutationsSubsetInPlace start len arr =
        seq {
            yield arr

            // c acts as the stack state.
            // i acts similarly to a stack pointer
            let c = Array.zeroCreate len
            let mutable i = 1

            while i < len do
                if c[i] < i then
                    if i % 2 = 0 then
                        arr |> swap start (start + i)
                    else
                        arr |> swap (start + c[i]) (start + i)

                    yield arr

                    // Swap has occurred ending the for-loop. Simulate the increment of the for-loop counter
                    c[i] <- c[i] + 1
                    // Simulate recursive call reaching the base case by bringing the pointer to the base case analog in the array
                    i <- 1
                else
                    // Calling generate(i+1, A) has ended as the for-loop terminated. Reset the state and simulate popping the stack by incrementing the pointer.
                    c[i] <- 0
                    i <- i + 1
        }

    /// Generates all permutations of an array of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Warning: this function permutes the source array in-place.
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    let inline permutationsInPlace arr =
        permutationsSubsetInPlace 0 (arr |> Array.length) arr

    /// Generates all permutations of an array of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    let inline permutations arr =
        arr |> Array.copy |> permutationsInPlace

    /// Generates all permutations of a subset of an array of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    ///
    /// # Parameters
    /// - start: The starting index of the elements to permute.
    /// - len: The number of elements to permute.
    /// - arr: The source array.
    let inline permutationsSubset start len arr =
        arr |> Array.copy |> permutationsSubsetInPlace start len

module Seq =
    /// Generates all permutations of a subset of a sequence of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Note: this function immediately enumerates the source sequence.
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    ///
    /// # Parameters
    /// - start: The starting index of the elements to permute.
    /// - len: The number of elements to permute.
    /// - s: The source sequence.
    let permutationsSubset start len s =
        s |> Seq.toArray |> Array.permutationsSubsetInPlace start len |> Seq.cast<_ seq>

    /// Generates all permutations of a sequence of elements using
    /// Heap's algorithm (https://en.wikipedia.org/wiki/Heap%27s_algorithm)
    ///
    /// Note: this function immediately enumerates the source sequence.
    ///
    /// Use with care. Although this function can enumerate sequences of
    /// arbitrary length, the number of permutation of a set of length
    /// `n` is `n!`. Thus a set with 13 items has 6.2x10^9 permutations
    /// and takes about 2.5 minutes to enumerate (~41.5 million enums/s).
    let permutations s =
        s |> Seq.toArray |> Array.permutationsInPlace |> Seq.cast<_ seq>

    /// Returns a sequence containing all **unique** pairs of elements from the
    /// input sequence.
    ///
    /// Pair (a,b) is considered the same as (b,a) so only the first is included.
    ///
    /// If `includeIdentity` is true, then identity pairs are included, e.g., (a,a), (b,b), etc..
    let allPairs includeIdentity (values: seq<_>) =
        seq {
            let arr =
                match values with
                | :? (_ array) as arr -> arr
                | _ -> values |> Seq.toArray

            for i = 0 to arr.Length - 1 do
                let h = arr[i]

                if includeIdentity then
                    yield h, h

                for j = i + 1 to arr.Length - 1 do
                    yield h, arr[j]
        }

    /// Generates all permutations of the given length from the input sequence.
    /// - If length is 0, returns the null set (one empty sequence).
    /// - If length is 1, returns each element as a singleton sequence.
    /// - If length > values.Length, returns empty sequence.
    let rec allPermutationsOfLength length (values: seq<_>) =
        match length with
        | 0 -> Seq.singleton Seq.empty
        | 1 -> values |> Seq.map Seq.singleton
        | _ ->
            let arr =
                match values with
                | :? (_[]) as arr -> arr
                | _ -> values |> Seq.toArray

            let excluding idx (arr: _[]) =
                seq {
                    for i = 0 to arr.Length - 1 do
                        if i <> idx then
                            yield arr[i]
                }

            if length > arr.Length then
                Seq.empty // no combinations possible
            else
                seq {
                    for i = 0 to arr.Length - 1 do
                        let subset = arr |> excluding i

                        for subPermutations in allPermutationsOfLength (length - 1) subset do
                            yield Seq.append (Seq.singleton arr[i]) subPermutations
                }

    /// Generates all _unique_ subsets of the specified length.
    /// Each subset is in the same order as the input sequence.
    /// - If length is 0, returns the null set (one empty sequence).
    /// - If length is 1, returns each element as a singleton sequence.
    /// - If length > values.Length, returns empty sequence.
    let rec allSubsetsOfLength length (values: seq<_>) =
        match length with
        | 0 -> Seq.singleton Seq.empty
        | 1 -> values |> Seq.map Seq.singleton
        | _ ->
            let arr =
                match values with
                | :? (_[]) as arr -> arr
                | _ -> values |> Seq.toArray

            if length > arr.Length then
                Seq.empty // no combinations possible
            else
                seq {
                    for i = 0 to arr.Length - 2 do
                        for minorSubset in allSubsetsOfLength (length - 1) arr[i + 1 ..] do
                            yield Seq.append (Seq.singleton arr[i]) minorSubset
                }

/// Simple algorithm for parsing a binary tree
type Tree<'V> =
    | Value of 'V
    | Branch of Tree<'V>[]

    [<TailCall>]
    override n.ToString() =
        let rec stringize sb =
            function
            | Branch(subnodes) ->
                sb |> StringBuilder.append "[" |> ignore

                subnodes
                |> Array.iteri (fun i n' ->
                    if i > 0 then
                        sb |> StringBuilder.append "," |> ignore

                    stringize sb n')

                sb |> StringBuilder.append "]" |> ignore
            | Value v -> sb.Append(sprintf "%A" v) |> ignore

        let sb = Text.StringBuilder()
        stringize sb n
        sb.ToString()

module Tree =

    type Token<'V> =
        | Leaf of 'V
        | StartBranch
        | NextBranch
        | EndBranch
        | EOF
        | Unknown

    /// Generates a simple tree.
    /// Here is an example:
    /// ```
    /// let tokenizer (input: string) idx =
    ///     if idx = -1 || idx = input.Length then
    ///         (EOF, -1)
    ///     else
    ///         match input[idx] with
    ///         | '[' -> (StartBranch, idx + 1)
    ///         | ',' -> (NextBranch, idx + 1)
    ///         | ']' -> (EndBranch, idx + 1)
    ///         | c when Char.IsLetter(c) -> (Leaf (string c), idx + 1)
    ///         | _ -> (Unknown, idx + 1)
    ///
    /// let tree = Tree.parse (tokenizer "[a,[b][,c,],]")
    /// ```
    let parse (tokenizer: int -> Token<'V> * int) =
        let rec parse idx starting =
            match tokenizer idx with
            | (Leaf v, idx) -> Some(Value v), idx

            | (StartBranch, idx) ->
                let mutable idx = idx

                let subnodes =
                    [| let mutable more = true

                       while more do
                           match parse idx false with
                           | Some subnode, i ->
                               yield subnode
                               idx <- i
                           | None, i ->
                               more <- false
                               idx <- i |]

                Some(Branch subnodes), idx

            | (EndBranch, _) when starting -> failwith $"Unexpected end at {idx}."
            | (EndBranch, idx) -> None, idx

            | (NextBranch, _) when starting -> failwith $"Unexpected token at {idx}."
            | (NextBranch, idx) -> parse idx false

            | (EOF, idx) when starting -> None, idx
            | (EOF, _) -> failwith $"Unexpected end."

            | (Unknown, idx) -> failwith $"Invalid token at {idx}."

        parse 0 true |> fst |> Option.get

    // Converts a tree to a string.
    let stringize (n: Tree<'V>) = n.ToString()

/// A basic 2-dimensional point.
[<Struct>]
// [<StructuredFormatDisplay("({x},{y})")>]
type Point2D =
    { x: int
      y: int }

    override this.ToString() = $"({this.x},{this.y})"

    static member zero = { x = 0; y = 0 }
    static member inline ofTuple(x, y) = { x = x; y = y }
    static member inline toTuple pt = (pt.x, pt.y)

    static member inline offset (dx, dy) pt = { x = pt.x + dx; y = pt.y + dy }

let (|Point2D|) = Point2D.ofTuple

/// A basic 3-dimensional point.
[<Struct>]
// [<StructuredFormatDisplay("({x},{y},{z})")>]
type Point3D =
    { x: int
      y: int
      z: int }

    override this.ToString() = $"({this.x},{this.y},{this.z})"

    static member zero = { x = 0; y = 0; z = 0 }
    static member inline ofTuple(x, y, z) = { x = x; y = y; z = z }
    static member inline toTuple pt = (pt.x, pt.y, pt.z)

    static member inline offset (dx, dy, dz) pt =
        { x = pt.x + dx
          y = pt.y + dy
          z = pt.z + dz }

let (|Point3D|) = Point3D.ofTuple
let (|Coords3D|) = Point3D.toTuple

/// A rectangle type.
///  p1+-----bottom----+
///    |               |
///   left           right
///    |               |
///    +------top------+p2
[<NoComparison>]
[<Struct>]
[<StructuredFormatDisplay("[{p1}..{p2}]")>]
type Rect =
    { p1: Point2D // the "smaller" point (bottom-left), inclusive
      p2: Point2D } // the "larger" point (top-right), exclusive

    override this.ToString() = $"[{this.p1}..{this.p2})"

    member this.left = this.p1.x
    member this.right = this.p2.x
    member this.bottom = this.p1.y
    member this.top = this.p2.y
    member this.width = this.right - this.left
    member this.height = this.top - this.bottom

    static member empty = { p1 = Point2D.zero; p2 = Point2D.zero }

    static member inline isEmpty(c: Rect) = c.width = 0 || c.height = 0

    /// Creates a normalized rect with the given points
    static member inline fromPoints p1 p2 = { p1 = p1; p2 = p2 } |> Rect.normalize

    /// Creates a normalized Rect with the given points
    static member inline fromCoords (Point2D xy1) (Point2D xy2) = Rect.fromPoints xy1 xy2

    static member inline dims(c: Rect) = (c.width, c.height)

    /// Returns volume of a rect.
    static member inline volume(c: Rect) =
        Math.Abs(int64 c.width * int64 c.height)

    /// Offsets the rect by offsetting both p1 and p2.
    static member inline offset (dx, dy) c : Rect =
        { c with
            p1 = c.p1 |> Point2D.offset (dx, dy)
            p2 = c.p2 |> Point2D.offset (dx, dy) }

    /// Grows the rect by offsetting p2.
    static member inline grow (dx, dy) c : Rect =
        { c with
            p2 = c.p2 |> Point2D.offset (dx, dy) }

    /// Returns a Rect with positive size in all dims (p1.xy <= p2.xy).
    static member normalize(c: Rect) =
        let c =
            if c.p1.x > c.p2.x then
                { c with
                    p1 = { c.p1 with x = c.p2.x - 1 }
                    p2 = { c.p2 with x = c.p1.x + 1 } }
            else
                c

        let c =
            if c.p1.y > c.p2.y then
                { c with
                    p1 = { c.p1 with y = c.p2.y - 1 }
                    p2 = { c.p2 with y = c.p1.y + 1 } }
            else
                c

        if Rect.isEmpty c then Rect.empty else c

    /// Checks if point intersects a Rect. Rect must be normalized.
    static member inline contains (pt: Point2D) (c: Rect) =
        c.left <= pt.x && pt.x < c.right && c.bottom <= pt.y && pt.y < c.top

    /// Returns the union of the two rects. Rect must be normalized.
    static member union (c1: Rect) (c2: Rect) =
        let x1, x2 = (c1, c2) ||> swapIf (c2.left < c1.left)

        let y1, y2 = (c1, c2) ||> swapIf (c2.bottom < c1.bottom)

        let inline min a b = if a < b then a else b
        let inline max a b = if a > b then a else b

        { p1 =
            { x = min x1.left x2.left
              y = min y1.bottom y2.bottom }
          p2 =
            { x = max x1.right x2.right
              y = max y1.top y2.top } }

    /// Find intersection of two rects. Rects must be normalized.
    static member intersection (c1: Rect) (c2: Rect) =
        let x1, x2 = (c1, c2) ||> swapIf (c2.left < c1.left)

        let y1, y2 = (c1, c2) ||> swapIf (c2.bottom < c1.bottom)

        let inline min a b = if a < b then a else b

        // must intersect in all dims
        if x2.left < x1.right && y2.bottom < y1.top then
            { p1 = { x = x2.left; y = y2.bottom }
              p2 =
                { x = min x1.right x2.right
                  y = min y1.top y2.top } }
        else
            Rect.empty

    static member inline intersects c1 c2 =
        not (Rect.intersection c1 c2 |> Rect.isEmpty)

    /// Returns `a` minus the intersection of `b`, or `None` if there is no intersection.
    /// Rects must be normalized.
    static member tryDifference a b =
        match Rect.intersection a b with
        | i when i |> Rect.isEmpty -> None
        | i ->
            [ let mutable a = a

              if i.right = b.right && a.right <> b.right then
                  yield
                      { a with
                          p1 = { a.p1 with x = i.right } }

                  a <-
                      { a with
                          p2 = { a.p2 with x = i.right } }

              if i.left = b.left && a.left <> b.left then
                  yield { a with p2 = { a.p2 with x = i.left } }
                  a <- { a with p1 = { a.p1 with x = i.left } }

              if i.top = b.top && a.top <> b.top then
                  yield { a with p1 = { a.p1 with y = i.top } }
                  a <- { a with p2 = { a.p2 with y = i.top } }

              if i.bottom = b.bottom && a.bottom <> b.bottom then
                  yield
                      { a with
                          p2 = { a.p2 with y = i.bottom } }

                  a <-
                      { a with
                          p1 = { a.p1 with y = i.bottom } } ]
            |> Some

/// A cube type.
[<NoComparison>]
[<Struct>]
[<StructuredFormatDisplay("[{p1}..{p2}]")>]
type Cube =
    { p1: Point3D // the "smaller" point, inclusive
      p2: Point3D } // the "larger" point, exclusive

    override this.ToString() = $"[{this.p1}..{this.p2})"

    member this.left = this.p1.x
    member this.right = this.p2.x
    member this.bottom = this.p1.y
    member this.top = this.p2.y
    member this.back = this.p1.z
    member this.front = this.p2.z
    member this.width = this.right - this.left
    member this.height = this.top - this.bottom
    member this.depth = this.front - this.back

    static member empty = { p1 = Point3D.zero; p2 = Point3D.zero }

    static member inline isEmpty(c: Cube) =
        c.width = 0 || c.height = 0 || c.depth = 0

    /// Creates a normalized cube with the given points
    static member inline fromPoints p1 p2 = { p1 = p1; p2 = p2 } |> Cube.normalize

    /// Creates a normalized cube with the given points
    static member inline fromCoords (Point3D xyz1) (Point3D xyz2) = Cube.fromPoints xyz1 xyz2

    static member inline dims(c: Cube) = (c.width, c.height, c.depth)

    /// Returns volume of a cube.
    static member inline volume(c: Cube) =
        Math.Abs(int64 c.width * int64 c.height * int64 c.depth)

    /// Offsets the cube by offsetting both p1 and p2.
    static member inline offset (dx, dy, dz) c : Cube =
        { c with
            p1 = c.p1 |> Point3D.offset (dx, dy, dz)
            p2 = c.p2 |> Point3D.offset (dx, dy, dz) }

    /// Grows the cube by offsetting p2.
    static member inline grow (dx, dy, dz) c : Cube =
        { c with
            p2 = c.p2 |> Point3D.offset (dx, dy, dz) }

    /// Returns a Cube with positive size in all 3 dims (p1.xyz <= p2.xyz).
    static member normalize(c: Cube) =
        let c =
            if c.p1.x > c.p2.x then
                { c with
                    p1 = { c.p1 with x = c.p2.x - 1 }
                    p2 = { c.p2 with x = c.p1.x + 1 } }
            else
                c

        let c =
            if c.p1.y > c.p2.y then
                { c with
                    p1 = { c.p1 with y = c.p2.y - 1 }
                    p2 = { c.p2 with y = c.p1.y + 1 } }
            else
                c

        let c =
            if c.p1.z > c.p2.z then
                { c with
                    p1 = { c.p1 with z = c.p2.z - 1 }
                    p2 = { c.p2 with z = c.p1.z + 1 } }
            else
                c

        if Cube.isEmpty c then Cube.empty else c

    /// Checks if point intersects a cube. Cube must be normalized.
    static member inline contains pt (c: Cube) =
        c.left <= pt.x
        && pt.x < c.right
        && c.bottom <= pt.y
        && pt.y < c.top
        && c.back <= pt.z
        && pt.z < c.front

    /// Returns the union of the two cubes. Cube must be normalized.
    static member union (c1: Cube) (c2: Cube) =
        let x1, x2 = (c1, c2) ||> swapIf (c2.left < c1.left)

        let y1, y2 = (c1, c2) ||> swapIf (c2.bottom < c1.bottom)

        let z1, z2 = (c1, c2) ||> swapIf (c2.back < c1.back)

        let inline min a b = if a < b then a else b
        let inline max a b = if a > b then a else b

        { p1 =
            { x = min x1.left x2.left
              y = min y1.bottom y2.bottom
              z = min z1.back z2.back }
          p2 =
            { x = max x1.right x2.right
              y = max y1.top y2.top
              z = max z1.front z2.front } }

    /// Find intersection of two cubes. Cubes must be normalized.
    static member intersection (c1: Cube) (c2: Cube) =
        let x1, x2 = (c1, c2) ||> swapIf (c2.left < c1.left)

        let y1, y2 = (c1, c2) ||> swapIf (c2.bottom < c1.bottom)

        let z1, z2 = (c1, c2) ||> swapIf (c2.back < c1.back)

        let inline min a b = if a < b then a else b

        // must intersect in all dims
        if x2.left < x1.right && y2.bottom < y1.top && z2.back < z1.front then
            { p1 =
                { x = x2.left
                  y = y2.bottom
                  z = z2.back }
              p2 =
                { x = min x1.right x2.right
                  y = min y1.top y2.top
                  z = min z1.front z2.front } }
        else
            Cube.empty

    static member inline intersects c1 c2 =
        not (Cube.intersection c1 c2 |> Cube.isEmpty)

    /// Returns `a` minus the intersection of `b`, or `None` if there is no intersection.
    /// Cubes must be normalized.
    static member tryDifference a b =
        match Cube.intersection a b with
        | i when i |> Cube.isEmpty -> None
        | i ->
            [ let mutable a = a

              if i.right = b.right && a.right <> b.right then
                  yield
                      { a with
                          p1 = { a.p1 with x = i.right } }

                  a <-
                      { a with
                          p2 = { a.p2 with x = i.right } }

              if i.left = b.left && a.left <> b.left then
                  yield { a with p2 = { a.p2 with x = i.left } }
                  a <- { a with p1 = { a.p1 with x = i.left } }

              if i.top = b.top && a.top <> b.top then
                  yield { a with p1 = { a.p1 with y = i.top } }
                  a <- { a with p2 = { a.p2 with y = i.top } }

              if i.bottom = b.bottom && a.bottom <> b.bottom then
                  yield
                      { a with
                          p2 = { a.p2 with y = i.bottom } }

                  a <-
                      { a with
                          p1 = { a.p1 with y = i.bottom } }

              if i.front = b.front && a.front <> b.front then
                  yield
                      { a with
                          p1 = { a.p1 with z = i.front } }

                  a <-
                      { a with
                          p2 = { a.p2 with z = i.front } }

              if i.back = b.back && a.back <> b.back then
                  yield { a with p2 = { a.p2 with z = i.back } }
                  a <- { a with p1 = { a.p1 with z = i.back } } ]
            |> Some

let manhattanDistance (p1: Point2D) (p2: Point2D) =
    Math.Abs(p2.x - p1.x) + Math.Abs(p2.y - p1.y)

let squareDistance (p1: Point2D) (p2: Point2D) =
    (p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y)

let distance (p1: Point2D) (p2: Point2D) =
    squareDistance p1 p2 |> float |> Math.Sqrt

let manhattanDistance3D (p1: Point3D) (p2: Point3D) =
    Math.Abs(p2.x - p1.x) + Math.Abs(p2.y - p1.y) + Math.Abs(p2.z - p1.z)

let squareDistance3D (p1: Point3D) (p2: Point3D) =
    int64 (p2.x - p1.x) * int64 (p2.x - p1.x)
    + int64 (p2.y - p1.y) * int64 (p2.y - p1.y)
    + int64 (p2.z - p1.z) * int64 (p2.z - p1.z)

let distance3D (p1: Point3D) (p2: Point3D) =
    squareDistance3D p1 p2 |> float |> Math.Sqrt
    
/// A rectangular grid of items, modeled as an array of rows.
/// The Y coordinate specifies a row index.
/// The X coordinate specifies a column index.
type Grid<'T> = GridRow<'T>[]
and GridRow<'T> = 'T[]

module Grid =
    open System.Collections.Generic
    /// The X-Y coordinates of a grid position.
    type Coordinates = Coordinate2D

    /// Asserts that the grid is rectangular.
    let verify (grid: Grid<'T>) =
        for y = 1 to grid.Length - 1 do
            assert (grid[y].Length = grid[0].Length)

    let inline create (width: XCoord) (height: YCoord) value : Grid<'T> =
        Array.init height (fun _ -> Array.create width value)

    let inline init (width: XCoord) (height: YCoord) initializer : Grid<'T> =
        Array.init height (fun y -> Array.init width (fun x -> initializer x y))

    let inline clone (grid: Grid<'T>) = grid |> Array.map Array.copy

    let inline copy (dst: Grid<'T>) (src: Grid<'T>) =
        if dst.Length <> src.Length || dst[0].Length <> src[0].Length then
            invalidArg (nameof dst) "Source and Destination grid must have the same dimensions."

        for i = 0 to src.Length - 1 do
            Array.Copy(src[i], 0, dst[i], 0, src[i].Length)

    /// Converts an array of strings into a character grid.
    let fromLines (lines: string[]) : Grid<char> =
        lines |> Array.map String.toArray |> tee verify

    /// Returns the width (X direction) of a grid.
    let inline width (grid: Grid<'T>) : XCoord = grid[0].Length

    /// Returns the height (Y direction) of a grid.
    let inline height (grid: Grid<'T>) : YCoord = grid.Length

    /// Returns the width and height of a grid.
    let inline widthAndHeight grid = (width grid, height grid)

    /// Returns the grid item at the X,Y coordinates. Throws exception if coordinates are out of bounds
    let inline item (x: XCoord) (y: YCoord) (grid: Grid<'T>) = grid[y][x]

    /// Returns the grid item at the X,Y coordinates, or the default value if the coordinates are out of bounds.
    let tryItem (x: XCoord) (y: YCoord) (grid: Grid<'T>) =
        if y < 0 || x < 0 || y >= grid.Length || x >= grid[y].Length then
            None
        else
            Some(grid |> item x y)

    let inline tryItemV (x: XCoord) (y: YCoord) (grid: Grid<'T>) =
        if y < 0 || x < 0 || y >= grid.Length || x >= grid[y].Length then
            ValueNone
        else
            ValueSome(grid |> item x y)

    let inline itemOrDefault (x: XCoord) (y: YCoord) defValue (grid: Grid<'T>) =
        if y < 0 || x < 0 || y >= grid.Length || x >= grid[y].Length then
            defValue
        else
            grid |> item x y

    /// Returns the grid item at the given coordinates. Throws exception if coordinates are out of bounds
    let inline get ((x, y): Coordinates) (grid: Grid<'T>) = item x y grid
    /// Returns the grid item at the given coordinates, or None if the coordinates are out of bounds.
    let inline tryGet ((x, y): Coordinates) (grid: Grid<'T>) = tryItem x y grid
    /// Returns the grid item at the given coordinates, or ValueNone if the coordinates are out of bounds.
    let inline tryGetV ((x, y): Coordinates) (grid: Grid<'T>) = tryItemV x y grid
    /// Returns the grid item at the given coordinates, or the default value if the coordinates are out of bounds.
    let inline getOrDefault ((x, y): Coordinates) defValue (grid: Grid<'T>) = itemOrDefault x y defValue grid

    let inline next pos dir grid =
        let (x, y) = Direction.offset pos dir
        item x y grid

    let inline nextOrDefault pos dir def grid =
        let (x, y) = Direction.offset pos dir
        itemOrDefault x y def grid

    let inline tryNext pos dir grid =
        let (x, y) = Direction.offset pos dir
        tryItem x y grid

    let inline tryNextV pos dir grid =
        let (x, y) = Direction.offset pos dir
        tryItemV x y grid

    let inline set (x: XCoord) (y: YCoord) value (grid: Grid<'T>) = grid[y][x] <- value

    let inline trySet (x: XCoord) (y: YCoord) value (grid: Grid<'T>) =
        if y < 0 || x < 0 || y >= grid.Length || x >= grid[y].Length then
            ()
        else
            grid |> set x y value

    let rotate degrees (grid: Grid<'T>) : Grid<'T> =
        let (w, h) = grid |> widthAndHeight

        match (degrees % 360 + 360) % 360 with
        | 0 -> grid |> clone
        | 90 -> init h w (fun x y -> grid |> item (w - y - 1) x)
        | 180 -> init w h (fun x y -> grid |> item (w - x - 1) (h - y - 1))
        | 270 -> init h w (fun x y -> grid |> item y (h - x - 1))
        | _ -> invalidArg (nameof degrees) "Rotation angle must be a multiple of 90 degrees."

    /// Flip the grid in the specified orientation.
    let flip (orientation: Orientation) (grid: Grid<'T>) : Grid<'T> =
        let (w, h) = grid |> widthAndHeight

        match orientation with
        | Horizontal ->
            init w h (fun x y -> grid |> item (w - x - 1) y)
        | Vertical ->
            init w h (fun x y -> grid |> item x (h - y - 1))

    /// Reverses both the rows and columns of the grid.
    /// This is equivalent to rotating the grid 180 degrees.
    let inline rev (grid: Grid<'T>) : Grid<'T> =
        grid |> rotate 180

    /// Returns the specified row of the grid.
    let inline row (y: YCoord) (grid: Grid<'T>) : 'T seq = grid[y]

    /// Returns the specified column of the grid.
    let col (x: XCoord) (grid: Grid<'T>) : 'T seq =
        seq {
            for y = 0 to grid.Length - 1 do
                grid |> item x y
        }

    /// Applies the given function to each item in the specified grid column. The `Coordinates` passed to the function indicates the X-Y coordinates.
    let inline iterCol (idx: XCoord) ([<InlineIfLambda>] fn: Coordinates -> 'T -> unit) (grid: Grid<'T>) =
        for y = 0 to grid.Length - 1 do
            fn (idx, y) (grid |> item idx y)

    /// Applies the given function to each item in the specified grid row. The `Coordinates` passed to the function indicates the X-Y coordinates.
    let inline iterRow (idx: YCoord) ([<InlineIfLambda>] fn: Coordinates -> 'T -> unit) (grid: Grid<'T>) =
        for x = 0 to grid[idx].Length - 1 do
            fn (x, idx) (grid |> item x idx)

    /// Applies the given function to each item of the grid. The `Coordinates` passed to the function indicates the X-Y coordinates of the item.
    let inline iter ([<InlineIfLambda>] fn: Coordinates -> 'T -> unit) (grid: Grid<'T>) =
        let (w, h) = grid |> widthAndHeight

        for y = 0 to h - 1 do
            for x = 0 to w - 1 do
                fn (x, y) (grid |> item x y)

    /// Builds a new grid whose items are the results of applying the given function to each of the items of the grid.
    /// The tuple passed to the function indicates the X-Y coordinates of item being transformed, starting at (0,0).
    let inline map ([<InlineIfLambda>] mapping: Coordinates -> 'T -> 'U) (grid: Grid<'T>) : Grid<'U> =
        grid
        |> Array.mapi (fun y row -> row |> Array.mapi (fun x item -> mapping (x, y) item))

    /// Counts the number of occurrences of the given item in the grid.
    let inline countOf (x: 'T) (grid: Grid<'T>) =
        grid 
        |> Array.sumBy (Array.fold (fun c item -> if item = x then c + 1 else c) 0)

    /// Builds a new grid whose items are the results of applying the given function to each of the items of the grid.
    /// The tuple passed to the function indicates the X-Y coordinates of item being transformed, starting at (0,0).
    [<TailCall>]
    let inline fold
        ([<InlineIfLambda>] folder: 'StateT -> Coordinates -> 'T -> 'StateT)
        (state: 'StateT)
        (grid: Grid<'T>)
        =
        let rec loop state x y =
            if y >= grid.Length then
                state
            elif x >= grid[y].Length then
                loop state 0 (y + 1)
            else
                let state = folder state (x, y) (grid |> item x y)
                loop state (x + 1) y

        loop state 0 0

    /// Returns the X-Y coordinates of the first item in the grid that satisfies the given predicate.
    /// If the item is not found, `None` is returned.
    [<TailCall>]
    let tryPick (predicate: Coordinates -> 'T -> 'U option) (grid: Grid<'T>) =
        let rec loop x y =
            if y >= grid.Length then
                None
            elif x >= grid[y].Length then
                loop 0 (y + 1)
            else
                match predicate (x, y) (grid |> item x y) with
                | None -> loop (x + 1) y
                | res -> res

        loop 0 0

    /// Returns the X-Y coordinates of the first item in the grid that satisfies the given predicate.
    /// Throws an exception is the item is not found.
    let pick predicate (grid: Grid<'T>) =
        grid
        |> tryPick predicate
        |> Option.defaultWith (fun _ -> failwith "Value not found")

    /// Returns the X-Y coordinates of the first item in the grid with the given value.
    /// If the item is not found, `None` is returned.
    let inline tryFind value (grid: Grid<'T>) =
        grid |> tryPick (fun coords v -> if v = value then Some coords else None)

    /// Returns the X-Y coordinates of the first item in the grid with the given value.
    /// Throws an exception is the item is not found.
    let find value (grid: Grid<'T>) =
        grid
        |> tryFind value
        |> Option.defaultWith (fun _ -> failwith "Value not found")

    let toSeq (grid: Grid<'T>) =
        seq {
            for row in grid do
                yield! row
        }

    /// Converts the grid to a sequence of (Coord, 'T) tuples.
    let inline toCoordSeq (grid: Grid<'T>) : seq<Coordinates * 'T> =
        seq {
            let (w, h) = grid |> widthAndHeight

            for y = 0 to h - 1 do
                for x = 0 to w - 1 do
                    yield (x, y), (grid |> item x y)
        }

    /// Breadth-first flood fill. Stack-safe at the expense of speed and memory.
    let floodFn x y fn grid =
        let oldValue = grid |> item x y

        let rec bfs (points: Queue<_>) =
            match points.TryDequeue() with
            | false, _ -> () // done
            | true, (x, y) ->
                match grid |> tryItemV x y with
                | ValueSome n when n = oldValue ->
                    let newValue = fn (x, y)

                    if newValue <> oldValue then
                        grid |> set x y newValue
                        // add neighbors to queue
                        points.Enqueue((x + 1, y))
                        points.Enqueue((x - 1, y))
                        points.Enqueue((x, y + 1))
                        points.Enqueue((x, y - 1))
                | _ -> ()

                bfs points // tail-recursive

        Queue [ (x, y) ] |> bfs

    /// Breadth-first flood fill. Stack-safe at the expense of speed and memory.
    let flood x y color grid =
        match grid |> tryItemV x y with
        | ValueNone -> () // out of bounds. nothing to do
        | ValueSome oldValue when oldValue = color -> () // nothing to do
        | ValueSome oldValue ->
            let w, h = grid |> widthAndHeight

            let rec bfs (points: Queue<_>) =
                match points.TryDequeue() with
                | false, _ -> () // done
                | true, (x, y) ->
                    match grid |> item x y with
                    | n when n = oldValue ->
                        grid |> set x y color

                        // flood the row to take advantage of cache consistency
                        let mutable x2 = x + 1

                        while x2 < w do
                            match grid |> item x2 y with
                            | n when n = oldValue ->
                                grid |> set x2 y color
                                x2 <- x2 + 1
                            | _ -> x2 <- w

                        x2 <- x - 1

                        while x2 >= 0 do
                            match grid |> item x2 y with
                            | n when n = oldValue ->
                                grid |> set x2 y color
                                x2 <- x2 - 1
                            | _ -> x2 <- -1

                        // queue points in rows above and below
                        if y > 0 then
                            points.Enqueue((x, y - 1))

                        if y < h - 1 then
                            points.Enqueue((x, y + 1))
                    | _ -> ()

                    bfs points // tail-recursive

            Queue [ (x, y) ] |> bfs

    /// Convert a grid to a string.
    /// Element are formatted using the given formatting function
    /// and separated with the given separator string.
    let stringizeFmt (format: 'T -> string) (separator: string) (grid: Grid<'T>) =
        let (w, h) = grid |> widthAndHeight

        let itemWidth =
            let ty = typeof<'T>

            if Type.op_Equality (ty, typeof<char>) then 1
            elif Type.op_Equality (ty, typeof<byte>) then 2 // assume hex format
            else 4 // a guess

        let sb = StringBuilder.create (itemWidth * w * h * (1 + separator.Length))

        for y = 0 to h - 1 do
            for x = 0 to w - 1 do
                sb.Append(grid |> item x y |> format).Append(separator) |> ignore

            sb.AppendLine() |> ignore

        sb.ToString()

    /// Convert a grid to a string.
    /// Element are formatted using ToString() unless and separated with the given separator string.
    let inline stringize separator (grid: Grid<'T>) =
        let formatter =
            let ty = typeof<'T>

            if Type.op_Equality (ty, typeof<byte>) then
                fun b -> (byte b).ToString("02X") // use hex format
            else
                string // mostly same as obj.ToString()

        stringizeFmt formatter separator grid

    /// Print the grid to stdout. Element are separated with the given separator string.
    let inline printfnSep (separator: string) (grid: Grid<'T>) =
        grid |> stringize separator |> printfn "%s"

    /// Print the grid to stdout.
    let inline printfn grid = printfnSep "" grid

/// Splits a string of text into an array of individual lines (delimited by `\n`).
/// All lines are trimmed. Empty lines are _not_ discarded.
let (|TextLines|) = String.trim >> String.splitO "\n" StringSplitOptions.TrimEntries

/// Splits a string of text into an array of individual lines (delimited by `\n`).
/// All lines are trimmed. Empty lines _are_ discarded.
let parseInputText (TextLines text: string) =
    text |> Array.where (not << String.isEmpty)

/// Converts a collection of strings into an array of character arrays.
let toCharArrays (strings: string seq) =
    strings |> Seq.map String.toArray |> Seq.toArray

/// Splits a collection of strings into an array of word arrays. Words are
/// delimited by spaces (one or more).
let toWordArrays (strings: string seq) =
    strings
    |> Seq.map (String.splitO " " StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toArray

/// Splits a collection of strings into groups of strings. Each group begins
/// with a string matching the specified prefix.
let toGroups groupPrefix (strings: string[]) =
    if strings.Length > 0 && not (strings[0] |> String.startsWith groupPrefix) then
        invalidArg (nameof groupPrefix) (sprintf "First string must start with %A" strings[0])

    let mutable idx = 0

    [| while idx < strings.Length do
           let groupName = strings[idx]
           idx <- idx + 1

           groupName,
           [| while idx < strings.Length && not (strings[idx] |> String.startsWith groupPrefix) do
                  strings[idx]
                  idx <- idx + 1 |] |]

/// Performs a binary search over the range of integers using the given
/// predicate to home-in on the desired value.
///
/// The function starts by calling the predicate using the middle value
/// from the range (n..m) inclusive. If the predicate returns 0 the search
/// is over and the function returns the middle value. Otherwise, if the
/// predicate return < 0, the function searches the left half of the range,
/// else the right half. If it runs out of integers to search, None is returned.
[<TailCall>]
let rec binarySearch n m pred =
    if n > m then
        binarySearch m n pred
    else // n <= m
        let mid = n + ((m - n) / 2)

        match pred mid with
        | 0 -> Some mid // found
        // not found
        | _ when n = m -> None
        | x when x < 0 ->
            // search left
            binarySearch n (mid - 1) pred
        | _ ->
            // search right
            binarySearch (mid + 1) m pred
