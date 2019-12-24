module day24
open System.Collections.Generic

let input = "###.#
..#..
#..#.
#....
.#.#."

type Tile =
  | Open
  | Closed

let boardToBitmask width height (board: Tile array) =
  let mutable result = 0
  for y in 0..height-1 do
    for x in 0..width-1 do
      let index = x + y*width
      if board.[index] = Closed then
        let bit = pown 2 index
        result <- result ||| bit
  result

let evolve width height board =
  board
  |> Array.mapi (fun i s ->
    let y,x = System.Math.DivRem(i,width)
    let adjacentCount =
      [(x-1,y); (x+1,y); (x,y-1); (x,y+1)]
      |> List.filter (fun (nx,ny) -> nx>=0 && nx < width && ny>=0 && ny<height)
      |> List.filter (fun (nx,ny) -> board.[nx+ny*width] = Closed)
      |> List.length
    match board.[x+y*width] with
    | Open -> 
      if adjacentCount = 1 || adjacentCount = 2 then
        Closed
      else
        Open
    | Closed ->
      if adjacentCount = 1 then
        Closed
      else
        Open
  )

let drawBoard width height (board: Tile array) =
  for y in 0 .. height-1 do
    for x in 0 .. width-1 do
      let index = x + y*width
      match board.[index] with
      | Open -> printf "."
      | Closed -> printf "#"
    printf "\n"
  printf "\n"

[<EntryPoint>]
let main argv =
  let width = 5
  let height = 5
  let board =
    input.Split("\r\n")
    |> Array.collect 
      (fun l -> 
        l 
        |> Seq.map (fun t ->
          match t with
          | '.' -> Open
          | '#' -> Closed
          | x -> failwith "unknown tile type"
        )
        |> Seq.toArray
      )

  let seenBoards = HashSet<int>()

  let rec findFirstDuplicate board =
    let newBoard = evolve width height board
    drawBoard width height newBoard
    let newMask = boardToBitmask width height newBoard
    if seenBoards.Contains newMask then
      newBoard
    else
      seenBoards.Add newMask |> ignore
      findFirstDuplicate newBoard

  let dupBoard = findFirstDuplicate board


  printfn "%A" (boardToBitmask width height dupBoard)
  0 // return an integer exit code
