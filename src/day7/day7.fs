module day7

let program = "3,8,1001,8,10,8,105,1,0,0,21,30,55,76,97,114,195,276,357,438,99999,3,9,102,3,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,2,9,1001,9,2,9,102,2,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,1001,9,4,9,102,5,9,9,101,4,9,9,1002,9,4,9,4,9,99,3,9,101,2,9,9,102,4,9,9,1001,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"

let testProgram1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let testProgram2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
let testProgram3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

type Result =
  | MoveForward of int
  | Jump of int
  | Finished

type ParameterMode =
  | Address
  | Immediate

let rec parseParameterModes pm =
  if pm = 0 then 
    []
  else
    match pm % 10 with
    | 0 -> [Address] @ parseParameterModes (pm/10)
    | 1 -> [Immediate] @ parseParameterModes (pm/10)
    | x -> failwith <| sprintf "Unknown parameter mode: %A" x

let parseInstruction i =
  let opcode = i % 100
  let parameterModes = parseParameterModes (i/100)
  (opcode, parameterModes )

let getValue (pos: int) parameterMode (memory: int[]) =
  match parameterMode with
  | Address -> memory.[pos]
  | Immediate -> pos

let getParameterMode (parameterModes: ParameterMode list) index =
  List.tryItem index parameterModes
  |> Option.defaultValue Address

let handleOpcode (input: unit -> string) (output: string -> unit) (memory: int[]) pos =
  let (opcode, parameterModes) = parseInstruction memory.[pos]
  let gp = getParameterMode parameterModes
  //printfn "opcode %A, parameterModes %A" opcode parameterModes
  match opcode with
  | 1 -> // Addition
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    let r = memory.[pos+3]
    let sum = a + b
    memory.[r] <- sum
    MoveForward 4
  | 2 -> // Multiply
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    let r = memory.[pos+3]
    let mul = a * b
    memory.[r] <- mul
    MoveForward 4
  | 3 -> // Read
    let a = input() |> int
    printfn "Please input value: %A" a
    let r = memory.[pos+1]
    memory.[r] <- a
    MoveForward 2
  | 4 -> // Write
    let a = getValue (memory.[pos+1]) (gp 0) memory
    output (string a)
    MoveForward 2
  | 5 -> // jump if true
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    if a > 0 then
      Jump b
    else
      MoveForward 3
  | 6 -> // jump if false
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    if a = 0 then
      Jump b
    else
      MoveForward 3
  | 7 -> // less than
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    let c = memory.[pos+3]
    if a < b then
      memory.[c] <- 1
    else
      memory.[c] <- 0
    MoveForward 4
  | 8 -> // less than
    let a = getValue (memory.[pos+1]) (gp 0) memory
    let b = getValue (memory.[pos+2]) (gp 1) memory
    let c = memory.[pos+3]
    if a = b then
      memory.[c] <- 1
    else
      memory.[c] <- 0
    MoveForward 4
  | 99 -> Finished
  | x -> failwith (sprintf "Unknown opcode: %A, pos: %A, memory: %A" x pos memory )

let rec run input output memory pos =
  let result = handleOpcode input output memory pos
  match result with
  | Finished -> ()
  | MoveForward x -> run input output memory (pos+x)
  | Jump x -> run input output memory x

let createInput input =
  let mutable xs = input
  fun () ->
    let i = List.head xs
    xs <- List.tail xs
    i

let swap<'a> (a: 'a[]) p p2 =
    let t = a.[p]
    a.[p] <- a.[p2]
    a.[p2] <- t

let getPermutations =
  let input = seq {0..4} |> Seq.toArray
  let rec heapsAlgorithm a n =
    if n = 1 then 
      [Array.copy a]
    else
      let r =
        [0..(n-2)]
        |> List.collect (fun i ->
          let result = heapsAlgorithm a (n-1)
          // always swap the first when odd,
          // swap the i-th when even
          if n % 2 = 0 then 
            swap a (n-1) i
          else 
            swap a (n-1) 0;
          result
        )
        
      heapsAlgorithm a (n-1)
      |> List.append r
  heapsAlgorithm input 5

[<EntryPoint>]
let main argv =
  let memory = 
    program.Split(",")
    |> Array.map int

  getPermutations
  |> List.map (fun ps ->
    printfn "==== %A ====" ps
    ps
    |> Array.fold
      (fun s psv ->
        let input = createInput [string psv; s]
        let mutable output = ""
        let outputf s =
          printfn "out -> %A" s
          output <- s

        run input outputf (Array.copy memory) 0
        output
      )
      "0"
  )
  |> List.maxBy (int)
  |> printfn "%A"
  0 // return an integer exit code
