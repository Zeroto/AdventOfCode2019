module day19
open System.Collections.Generic

let input = "109,424,203,1,21101,11,0,0,1105,1,282,21102,18,1,0,1105,1,259,2102,1,1,221,203,1,21102,1,31,0,1106,0,282,21101,38,0,0,1105,1,259,21001,23,0,2,21201,1,0,3,21101,0,1,1,21101,0,57,0,1105,1,303,1201,1,0,222,20102,1,221,3,20101,0,221,2,21101,259,0,1,21102,80,1,0,1106,0,225,21101,127,0,2,21102,91,1,0,1106,0,303,1201,1,0,223,20102,1,222,4,21101,259,0,3,21101,0,225,2,21102,225,1,1,21102,1,118,0,1106,0,225,21001,222,0,3,21101,0,89,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,0,148,0,1105,1,259,2102,1,1,223,21002,221,1,4,21001,222,0,3,21101,0,21,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,195,1,0,106,0,108,20207,1,223,2,20102,1,23,1,21102,1,-1,3,21101,0,214,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1201,-4,0,249,22102,1,-3,1,21201,-2,0,2,22101,0,-1,3,21102,250,1,0,1105,1,225,21202,1,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2106,0,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21201,-2,0,3,21101,0,343,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22101,0,-4,1,21101,384,0,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21201,1,0,-4,109,-5,2105,1,0"

let runProgram (program:Dictionary<bigint, bigint>) x y =
  let cprogram = Dictionary(program)

  let mutable input = [string x;string y]
  let inputf () =
    let s = input.Head
    input <- input.Tail
    Some s

  let mutable output = ""
  let outputf s =
    output <- s

  let mutable finished = false
  let mutable ip = bigint 0
  let mutable relBase = bigint 0
  let log _ = ()
  while not finished do
    let (f, p, b) = IntCode.run inputf outputf cprogram ip relBase log
    finished <- finished || f
    ip <- p
    relBase <- b
  
  output

[<EntryPoint>]
let main argv =
  System.Console.Clear()
  System.Console.CursorVisible <- false

  let program =
    input.Split(",")
    |> Array.map bigint.Parse
    |> Array.indexed
    |> Array.map (fun (k,v) -> KeyValuePair(bigint k, v))
    |> Dictionary

  let width = 100
  let height = 100
  
  let board = Array.zeroCreate (width*height)

  for y in 99 .. 99 do
    for x in 0 .. 99 do
      let cprogram = Dictionary(program)

      let mutable pos = -1;
      let inputf () =
        pos <- pos + 1
        if pos % 2 = 0 then
          x |> string |> Some
        else
          y |> string |> Some

      let outputf s = 
        board.[x + y*width] <- int s
        // printf "%s" s
        // if x = 99 then printf "\n"

      let mutable finished = false
      let mutable ip = bigint 0
      let mutable relBase = bigint 0
      let log _ = ()
      while not finished do
        let (f, p, b) = IntCode.run inputf outputf cprogram ip relBase log
        finished <- finished || f
        ip <- p
        relBase <- b

  board
  |> Array.filter ((=) 1)
  |> Array.length
  |> printfn "%A"

  // trace along 1 line until we find the other corner
  let mutable x =
    board
    |> Seq.skip ((height-1)*width)
    |> Seq.findIndex ((=) 1)

  let mutable found = false
  let mutable y = 99
  while not found do
    let o = runProgram program x y
    if o = "0" then
      // move right
      x <- x+1
    else
      // check corner
      let o = runProgram program (x+99) (y-99)
      if o = "1" then
        found <- true
      else
        y <- y+1
  printfn "%A %A" x y

  for y in 1063 .. 1165 do
    for x in 882 .. 984 do
      runProgram program (x) (y)
      |> printf "%s"
      if x = 984 then
        printf "\n"

  0 // return an integer exit code
