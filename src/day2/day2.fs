module day2

let input = "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,9,23,1,5,23,27,1,27,9,31,1,6,31,35,2,35,9,39,1,39,6,43,2,9,43,47,1,47,6,51,2,51,9,55,1,5,55,59,2,59,6,63,1,9,63,67,1,67,10,71,1,71,13,75,2,13,75,79,1,6,79,83,2,9,83,87,1,87,6,91,2,10,91,95,2,13,95,99,1,9,99,103,1,5,103,107,2,9,107,111,1,111,5,115,1,115,5,119,1,10,119,123,1,13,123,127,1,2,127,131,1,131,13,0,99,2,14,0,0"

let input2 = "1,0,0,0,99"

type Result =
  | MoveForward of int
  | Finished

let handleOpcode (memory: int[]) pos =
  match memory.[pos] with
  | 1 -> 
    let a = memory.[pos+1]
    let b = memory.[pos+2]
    let r = memory.[pos+3]
    let sum = memory.[a] + memory.[b]
    memory.[r] <- sum
    MoveForward 4
  | 2 -> 
    let a = memory.[pos+1]
    let b = memory.[pos+2]
    let r = memory.[pos+3]
    let mul = memory.[a] * memory.[b]
    memory.[r] <- mul
    MoveForward 4
  | 99 -> Finished
  | x -> failwith (sprintf "Unknown opcode: %A, pos: %A, memory: %A" x pos memory )

let rec run memory pos =
  let result = handleOpcode memory pos
  match result with
  | Finished -> ()
  | MoveForward x -> run memory (pos+x)

let findSource memory =
  for x in 0 .. 99 do
    for y in 0 .. 99 do
      let workingMemory = Array.copy memory
      workingMemory.[1] <- x
      workingMemory.[2] <- y
      run workingMemory 0
      if (workingMemory.[0] = 19690720) then
        printfn "%A, %A" x y

[<EntryPoint>]
let main argv =
  let memory = 
    input.Split(",")
    |> Array.map int
  // run memory 0

  // printfn "%A" memory.[0]
  findSource memory
  0 // return an integer exit code
