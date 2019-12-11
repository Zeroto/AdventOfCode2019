module IntCode
open System.Collections.Generic

type Result =
  | MoveForward of bigint
  | Jump of bigint
  | WaitForInput
  | SetBase of bigint
  | Finished

type ParameterMode =
  | Address
  | Immediate
  | Relative

let rec parseParameterModes pm =
  if pm = 0 then 
    []
  else
    match pm % 10 with
    | 0 -> [Address] @ parseParameterModes (pm/10)
    | 1 -> [Immediate] @ parseParameterModes (pm/10)
    | 2 -> [Relative] @ parseParameterModes (pm/10)
    | x -> failwith <| sprintf "Unknown parameter mode: %A" x

let parseInstruction (i: bigint) =
  let pi = int i // opcodes are guaranteed not to be big integers
  let opcode = pi % 100
  let parameterModes = parseParameterModes (pi/100)
  (opcode, parameterModes )

let getMemoryAtAddress (memory: Dictionary<bigint,bigint>) address =
  let success, value = memory.TryGetValue address
  if success then
    value
  else
    bigint 0

let getValue (pos: bigint) (relativeBase: bigint) parameterMode (memory: Dictionary<bigint,bigint>) =
  match parameterMode with
  | Address -> getMemoryAtAddress memory pos
  | Immediate -> pos
  | Relative -> getMemoryAtAddress memory (bigint.Add(relativeBase, pos))

let getOutputValue (pos: bigint) (relativeBase: bigint) parameterMode (memory: Dictionary<bigint,bigint>) =
  match parameterMode with
  | Address -> pos
  | Immediate -> failwith "Immediate mode is not supported for output values"
  | Relative -> relativeBase + pos

let getParameterMode (parameterModes: ParameterMode list) index =
  List.tryItem index parameterModes
  |> Option.defaultValue Address

let handleOpcode (input: unit -> string option) (output: string -> unit) (memory: Dictionary<bigint,bigint>) pos relativeBase log =
  let gm = getMemoryAtAddress memory
  let (opcode, parameterModes) = parseInstruction (gm pos)
  let gp = getParameterMode parameterModes
  //printfn "opcode %A, parameterModes %A" opcode parameterModes
  match opcode with
  | 1 -> // Addition
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    let r = getOutputValue (gm (bigint.Add(pos, bigint 3))) relativeBase (gp 2) memory    
    let sum = a + b
    memory.[r] <- sum
    MoveForward (bigint 4)
  | 2 -> // Multiply
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    let r = getOutputValue (gm (bigint.Add(pos, bigint 3))) relativeBase (gp 2) memory
    let mul = a * b
    memory.[r] <- mul
    MoveForward (bigint 4)
  | 3 -> // Read
    let a = input() |> Option.map (bigint.Parse)
    log <| sprintf " in <- %A" a
    let r = getOutputValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    match a with
    | Some a ->
      memory.[r] <- a
      MoveForward (bigint 2)
    | None ->
      WaitForInput
  | 4 -> // Write
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    output (string a)
    MoveForward (bigint 2)
  | 5 -> // jump if true
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    if a > bigint 0 then
      Jump b
    else
      MoveForward (bigint 3)
  | 6 -> // jump if false
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    if a = bigint 0 then
      Jump b
    else
      MoveForward (bigint 3)
  | 7 -> // less than
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    let c = getOutputValue (gm (bigint.Add(pos, bigint 3))) relativeBase (gp 2) memory
    if a < b then
      memory.[c] <- bigint 1
    else
      memory.[c] <- bigint 0
    MoveForward (bigint 4)
  | 8 -> // less than
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    let b = getValue (gm (bigint.Add(pos, bigint 2))) relativeBase (gp 1) memory
    let c = getOutputValue (gm (bigint.Add(pos, bigint 3))) relativeBase (gp 2) memory
    if a = b then
      memory.[c] <- bigint 1
    else
      memory.[c] <- bigint 0
    MoveForward (bigint 4)
  | 9 -> // set base
    let a = getValue (gm (bigint.Add(pos, bigint 1))) relativeBase (gp 0) memory
    SetBase (relativeBase + a)
  | 99 -> Finished
  | x -> failwith (sprintf "Unknown opcode: %A, pos: %A, memory: %A" x pos memory )

// returns true when program has ended
let rec run input output memory pos relativeBase log =
  let result = handleOpcode input output memory pos relativeBase log
  match result with
  | Finished -> (true, pos, relativeBase)
  | MoveForward x -> run input output memory (pos+x) relativeBase log
  | WaitForInput -> (false, pos, relativeBase)
  | SetBase x -> run input output memory (pos + bigint 2) x log
  | Jump x -> run input output memory x relativeBase log