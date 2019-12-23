module day22
open System.Text.RegularExpressions

let input = "deal with increment 34
cut 9781
deal with increment 20
cut 8981
deal with increment 11
cut -3391
deal with increment 15
cut 1485
deal with increment 10
cut 4826
deal into new stack
cut 1026
deal with increment 30
cut 1354
deal with increment 46
cut 1955
deal with increment 19
cut 1359
deal with increment 22
cut 9483
deal with increment 52
cut -2090
deal with increment 50
deal into new stack
cut -2205
deal with increment 69
cut -7934
deal with increment 11
cut 8311
deal with increment 42
cut -5430
deal with increment 57
deal into new stack
cut -2616
deal with increment 22
deal into new stack
cut 3540
deal with increment 38
cut -9097
deal with increment 37
cut -7014
deal with increment 26
cut 6983
deal with increment 11
deal into new stack
cut -4825
deal into new stack
cut -5791
deal with increment 19
cut -3577
deal with increment 6
deal into new stack
deal with increment 29
cut 7299
deal with increment 75
cut -8498
deal with increment 21
cut 5748
deal with increment 63
cut -344
deal with increment 5
cut -4306
deal with increment 65
cut 9431
deal with increment 7
cut 6825
deal with increment 28
deal into new stack
deal with increment 66
cut -1421
deal with increment 19
cut -8965
deal with increment 48
cut -5780
deal with increment 75
cut -3280
deal with increment 50
cut 6866
deal with increment 72
cut -5471
deal with increment 49
cut -8247
deal with increment 65
cut 3056
deal into new stack
deal with increment 39
cut 7011
deal with increment 48
cut -9660
deal with increment 56
cut -6843
deal into new stack
cut 5111
deal with increment 29
cut -7700
deal into new stack
deal with increment 23
cut -5263
deal with increment 61
deal into new stack"

// let testInput = "deal with increment 3"
let testInput = "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"

type Operation =
  | Reverse
  | Cut of int
  | IncrementDeal of int

let matchIncrementRegex = Regex("""deal with increment (\d+)""")
let (|MatchIncrement|_|) s =
  matchIncrementRegex.Match s
  |> (fun m ->
    if m.Success then
       m.Groups.[1].ToString() |> int |> Some
    else
      None
  )

let matchCutRegex = Regex("""cut (-?\d+)""")
let (|MatchCut|_|) s =
  matchCutRegex.Match s
  |> (fun m ->
    if m.Success then
      m.Groups.[1].ToString() |> int |> Some
    else
      None
  )

let (%+) (a:bigint) (b:bigint) = // force result to be positive
  ((a % b) + b) % b

type Move = bigint * bigint // y = ax+b

let compose size (m1: Move) (m2: Move) = // a(cx + d) + b == acx + ad + b
  let a,b = m1
  let c,d = m2
  ((a*c) %+ size, (a*d+b) %+ size)

let rec pow size (m: Move) (n: bigint) =
  if n = bigint 0 then
    (bigint 1, bigint 0)
  else
    let frac = n / bigint 2
    let rem = n % bigint 2
    let half = pow size m frac
    let halfSquare = compose size half half
    if rem = bigint 0 then
      halfSquare
    else
      compose size m halfSquare

let rec pow2 size (m: Move) (n: bigint) =
  if n = bigint 0 then
    (bigint 1, bigint 0)
  else if n = bigint 1 then
    m
  else
    let a = n / bigint 2
    let b = n % bigint 2
    let half = pow2 size m a
    if b = bigint 0 then
      compose size half half
    else
      compose size m (compose size half half)

let opToMove (size: bigint) =
  function
  | Reverse -> (bigint -1, size - bigint 1)
  | IncrementDeal i -> (bigint i, bigint 0)
  | Cut i -> (bigint 1, (bigint -i) %+ size)

// https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcodelib/NumberTheory.scala#L9
let MI (n: bigint) (g: bigint) =
  let rec helper s oldS t oldT r oldR =
    if (r = bigint 0) then
      oldS
    else
      let q = oldR / r
      helper (oldS - q*s) s (oldT - q*t) t (oldR - q*r) r

  (helper (bigint 0) (bigint 1) (bigint 1) (bigint 0) g n) %+ g

[<EntryPoint>]
let main argv =

  // let cardCount = 10007
  // let cardCount = 10
  let cardCount = bigint 119315717514047L

  let ops =
    input.Split("\r\n")
    |> Array.map (fun s ->
      match s with
      | "deal into new stack" -> Reverse
      | MatchIncrement i -> IncrementDeal i
      | MatchCut i -> Cut i
      | x -> failwith (sprintf "Unknown operation: %s" x)
    )

  let moves =
    ops
    |> Array.map (opToMove (cardCount))

  let combinedMove =
    moves
    |> Array.reduce (fun a b -> compose (cardCount) b a)

  //printfn "Result: %A" ((fst combinedMove * bigint 2019 + (snd combinedMove)) %+ (cardCount))
  
  printfn "pow: %A" (pow2 (bigint 10) (bigint 2, bigint 1) (bigint 2))

  let cardCount = bigint 119315717514047L
  let repeatedCombinedMove = pow2 cardCount combinedMove (bigint 101741582076661L)
  // let repeatedCombinedMove = pow cardCount (bigint 0, bigint 1) (bigint 101741582076661L)
  printfn "repeatedCombinedMove %A" repeatedCombinedMove
  let inverseMove =
    let a,b = repeatedCombinedMove
    let aInv = MI a cardCount
    (aInv, ((aInv * -b)) %+ cardCount)

  printfn "inverseMove %A" inverseMove
  
  printfn "Result part2: %A" ((fst inverseMove * bigint 2020 + (snd inverseMove)) %+ cardCount)

  // let deck = [0..cardCount-1]

  // let resultDeck =
  //   ops
  //   |> Array.fold (fun s op ->
  //     match op with
  //     | Reverse -> 
  //       s |> List.rev
  //     | Cut i ->
  //       let index =
  //         if i < 0 then
  //           cardCount+i
  //         else
  //           i
  //       s
  //       |> List.take index
  //       |> List.append (s |> List.skip index)
  //     | IncrementDeal inc ->
  //       let rec deal index (xs: int list) (output: int array) =
  //         let card = xs |> List.tryHead
  //         match card with
  //         | None -> output
  //         | Some c ->
  //           output.[index] <- c
  //           deal ((index+inc)%cardCount) (xs |> List.tail) output

  //       let output = Array.zeroCreate cardCount
  //       deal 0 s output
  //       |> Array.toList
  //   ) deck

  // // printfn "%A" (resultDeck |> List.map string |> String.concat ",")
  // printfn "%A" (resultDeck |> List.findIndex (fun x -> x = 2019))
  0 // return an integer exit code
