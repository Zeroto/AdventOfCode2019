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

let getAdjacentCoords (depth, x, y) =
  let mutable result = []
  if x = 0 then
    result <- (depth-1, 1, 2) :: result
  if x = 4 then
    result <- (depth-1, 3, 2) :: result
  if y = 0 then
    result <- (depth-1, 2, 1) :: result
  if y = 4 then
    result <- (depth-1, 2, 3) :: result
  
  if x = 1 && y = 2 then
    result <- (depth+1, 0, 0) :: (depth+1, 0, 1) :: (depth+1, 0, 2) :: (depth+1, 0, 3) :: (depth+1, 0, 4) :: result
  if x = 3 && y = 2 then
    result <- (depth+1, 4, 0) :: (depth+1, 4, 1) :: (depth+1, 4, 2) :: (depth+1, 4, 3) :: (depth+1, 4, 4) :: result
  if x = 2 && y = 1 then
    result <- (depth+1, 0, 0) :: (depth+1, 1, 0) :: (depth+1, 2, 0) :: (depth+1, 3, 0) :: (depth+1, 4, 0) :: result
  if x = 2 && y = 3 then
    result <- (depth+1, 0, 4) :: (depth+1, 1, 4) :: (depth+1, 2, 4) :: (depth+1, 3, 4) :: (depth+1, 4, 4) :: result

  [(depth, x-1,y); (depth, x+1,y); (depth, x,y-1); (depth, x,y+1)]
  |> List.filter (fun (_, nx,ny) -> nx>=0 && nx < 5 && ny>=0 && ny<5)
  |> List.append result

let emptyBoard = Array.create (5*5) Open

let evolve width height boards =
  let evolveBoard depth board =
    board
      |> Array.mapi (fun i t ->
        let y,x = System.Math.DivRem(i,width)
        if x = 2 && y = 2 then // center tile does not "exist"
          Open
        else
          let adjacentCoords = getAdjacentCoords (depth, x, y)

          let adjacentCount =
            adjacentCoords
            |> List.filter (fun (d,nx,ny) ->
              let board = Map.tryFind d boards |> Option.defaultValue emptyBoard
              board.[nx+ny*width] = Closed
            )
            |> List.length
          match t with
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

  let mutable newBoards =
    boards
    |> Map.map evolveBoard

  let minDepth = newBoards |> Map.toSeq |> Seq.minBy (fun (k,_) -> k) |> fst
  let maxDepth = newBoards |> Map.toSeq |> Seq.maxBy (fun (k,_) -> k) |> fst

  let minDepthBoard = evolveBoard (minDepth-1) emptyBoard
  let maxDepthBoard = evolveBoard (maxDepth+1) emptyBoard

  if minDepthBoard |> Array.exists (fun x -> x = Closed) then
    newBoards <- Map.add (minDepth-1) minDepthBoard newBoards

  if maxDepthBoard |> Array.exists (fun x -> x = Closed) then
    newBoards <- Map.add (maxDepth+1) maxDepthBoard newBoards

  newBoards

let drawBoard width height (board: Tile array) =
  for y in 0 .. height-1 do
    for x in 0 .. width-1 do
      let index = x + y*width
      match board.[index] with
      | Open -> printf "."
      | Closed -> printf "#"
    printf "\n"
  printf "\n"

let drawBoards iteration (boards: Map<int, Tile array>) =
  printfn "####### Iteration %d #######" iteration
  boards 
  |> Map.toSeq
  |> Seq.iter (fun (d,b) ->
    printfn "Depth: %d" d
    drawBoard 5 5 b
  )

[<EntryPoint>]
let main argv =
  let timer = System.Diagnostics.Stopwatch.StartNew()
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

  let recursiveBoards =
    Map.empty
    |> Map.add 0 board

  let rec findFirstDuplicate boards currentMinDepth currentMaxDepth iteration =
    //drawBoards iteration boards
    if iteration = 200 then
      boards
    else
      let newBoards = evolve 5 5 boards
      findFirstDuplicate newBoards currentMinDepth currentMaxDepth (iteration+1)


  let endBoard = findFirstDuplicate recursiveBoards 0 0 0


  printfn "%A" (endBoard |> Map.toSeq |> Seq.sumBy (fun (d, b) -> b |> Array.sumBy (fun t -> if t = Closed then 1 else 0)))
  printfn "took %dms" timer.ElapsedMilliseconds
  0 // return an integer exit code
