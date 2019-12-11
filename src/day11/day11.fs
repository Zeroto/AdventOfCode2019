module day11

open System.Collections.Generic

let input = "3,8,1005,8,325,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,28,2,3,7,10,2,1109,3,10,2,102,0,10,2,1005,12,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,67,2,109,12,10,1,1003,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,96,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,119,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,141,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,162,1,106,17,10,1006,0,52,1006,0,73,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,194,1006,0,97,1,1004,6,10,1006,0,32,2,8,20,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,102,1,8,231,1,1,15,10,1006,0,21,1,6,17,10,2,1005,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,267,2,1007,10,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,294,1006,0,74,2,1003,2,10,1,107,1,10,101,1,9,9,1007,9,1042,10,1005,10,15,99,109,647,104,0,104,1,21101,936333018008,0,1,21101,342,0,0,1106,0,446,21102,937121129228,1,1,21101,0,353,0,1105,1,446,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,209383001255,1,21102,400,1,0,1106,0,446,21101,0,28994371675,1,21101,411,0,0,1105,1,446,3,10,104,0,104,0,3,10,104,0,104,0,21101,867961824000,0,1,21101,0,434,0,1106,0,446,21102,1,983925674344,1,21101,0,445,0,1106,0,446,99,109,2,21201,-1,0,1,21102,40,1,2,21101,477,0,3,21102,467,1,0,1106,0,510,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,472,473,488,4,0,1001,472,1,472,108,4,472,10,1006,10,504,1101,0,0,472,109,-2,2106,0,0,0,109,4,1201,-1,0,509,1207,-3,0,10,1006,10,527,21102,1,0,-3,21202,-3,1,1,21201,-2,0,2,21102,1,1,3,21102,1,546,0,1106,0,551,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,574,2207,-4,-2,10,1006,10,574,22101,0,-4,-4,1105,1,642,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21101,0,593,0,1105,1,551,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,612,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,634,21201,-1,0,1,21101,634,0,0,105,1,509,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"

type Heading =
  | Up
  | Right
  | Down
  | Left

let getTile pos image =
  image
  |> Map.tryFind pos
  |> Option.defaultValue 0

let getNewHeading heading direction =
  match direction with
  | 0 -> //left
    match heading with
    | Up -> Left
    | Right -> Up
    | Down -> Right
    | Left -> Down
  | 1 -> //right
    match heading with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  | x -> failwith <| sprintf "Unknown direction %A" x

let move (x,y) heading direction =
  let newHeading = getNewHeading heading direction
  let newPos =
    match newHeading with
    | Up -> (x, y+1)
    | Right -> (x+1, y)
    | Down -> (x, y-1)
    | Left -> (x-1, y)
  (newPos, newHeading)

[<EntryPoint>]
let main argv =
  let program =
    input.Split(",")
    |> Array.map bigint.Parse
    |> Array.indexed
    |> Array.map (fun (k,v) -> KeyValuePair(bigint k, v))
    |> Dictionary

  let mutable image = Map.empty |> Map.add (0,0) 1
  let mutable currentPos = (0,0)
  let mutable currentHeading = Up
  let mutable output = []

  let inputf () =
    Some (string <| getTile currentPos image)

  let outputf s =
    output <- output @ [int s]
    if List.length output = 2 then
      image <- Map.add currentPos output.[0] image
      let (p,h) = move currentPos currentHeading output.[1]
      currentPos <- p
      currentHeading <- h
      output <- []

  let mutable finished = false
  let mutable ip = bigint 0
  let mutable relBase = bigint 0
  let log _ = ()
  while not finished do
    let (f, p, b) = IntCode.run inputf outputf program ip relBase log
    finished <- f
    ip <- p
    relBase <- b

  let drawImage image =
    let data =
      image
      |> Map.toArray
      |> Array.sort
    let (x1, y1, x2, y2) =
      data
      |> Array.fold
        (fun (x1, y1, x2, y2) ((x,y), _) ->
          (
            if x < x1 then x else x1
            , if y < y1 then y else y1
            , if x > x2 then x else x2
            , if y > y2 then y else y2
          )
        )
        (System.Int32.MaxValue, System.Int32.MaxValue, System.Int32.MinValue, System.Int32.MinValue)
    for y in y2 .. -1 .. y1 do
      for x in x1 .. x2 do
        Map.tryFind (x,y) image
        |> Option.defaultValue 0
        |> (fun x ->
          if x = 0 then
            System.Console.BackgroundColor <- System.ConsoleColor.Black
          else
            System.Console.BackgroundColor <- System.ConsoleColor.White
          printf " "
        )
      printf "\n"


  image
  |> Map.toArray
  |> Array.length
  |> printfn "%A"

  drawImage image
  0 // return an integer exit code
