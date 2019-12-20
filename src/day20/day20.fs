module day20
open System.Collections.Generic

let input = "###############################################################################################################
################################B#######C#########D#########E#####F###G#H######################################
##.#.#.#.#...#.....#...#.........#.....#.....#.....#.......#.....#.....#...#.#...#...#.#.............#.......##
##.#.#.#.###.#####.###.#########.#.###.#.#######.###.#.#####.#.###.###.#.#.#.#.#.#.###.#.#############.########
##...............#.#...#...........#...#...#.....#...#.#.....#.#.#.#...#.#.....#...#.#.......#...#.#...#...#.##
######.###.#####.#.###.###.###.#####.###.#####.#.#.#.###.#.###.#.###.#####.###.###.#.#.###.###.###.###.###.#.##
##.....#.#.#.#.....#.#.#.#...#.....#...#.....#.#.#.#...#.#.#...#.......#...#.#...#.#...#.......#.#.....#.#.#.##
########.###.#####.#.#.#.#.#.#########.#.#######.#.###.#.#####.#.#########.#.#.#####.###.#.#.#.#.#.#####.#.#.##
##.#...............#.#.....#.#.....#...#.....#.......#.#...#...#.....#...#.#...#...#...#.#.#.#.........#.....##
##.#.###.#.###.###.#.#.#.#######.#.#.#####.###########.#####.#.#.#.#####.###.#.#.###.#######.###########.######
##.#.#...#.#...#.#.....#.#.......#...#.....#...#.....#.#.....#.#.#.#.......#.#...........#.....#.#.#.#.#.....##
##.###########.#.#.#.###.#######.#######.#####.#.###.#.###.###.#.#########.###.#####.#.#.###.#.#.#.#.#.#.######
##.#.....#...#.#...#...#.#.#...#...#.......#.......#...#...#...#.....#.............#.#.#.#...#.#.....#.#...#.##
##.#.###.###.#.#.###.###.#.###.#.#######.#########.###########.#.#.###.###.###.#####.###############.#.#.###.##
##.....#.#.#...#.#...#.....#.#.....#.#.#.....#.#...#.......#...#.#...#.#...#...#.#.#.....#...#.#...#...#.....##
####.#####.###.#####.#######.#.#.###.#.###.###.#.#.###.#.#####.###.#######.###.#.#.#######.###.###.#.#####.####
##.........#...#.......#.......#.#.#...#.......#.#...#.#.....#.#.....#.....#.........#.#.#...#...#.......#...##
####.#####.###.#.#.###.#.###.#.#.#.#.#########.#.#######.###.#.#.#######.#####.###.###.#.#.###.###.#####.#.#.##
##...#.#.#.#.#.#.#.#...#.#.#.#.#.......#.#.....#.#.#...#.#.#.#.#.#.#.#.....#.#.#.#.#.........#...#.#.#.#...#.##
####.#.#.###.#######.#####.#.###.###.###.###.#.#.#.#.###.#.#.#.#.#.#.###.###.###.#####.#######.#####.#.###.####
##.....#...#...#.#.#.#.....#.#.....#.#.#.....#.#.....#.....#...#.....#...#.....#...#...#.....#.#...#.#.#.....##
####.###.###.#.#.#.#######.#####.#####.###.#.###.#.#.#####.#####.#######.###.###.###.#.###.###.###.#.#.###.#.##
##.......#.#.#.....#.#.#.#...#.#...#.#.....#.#.#.#.#.#.#.#.#...#...#.#...............#.#.#...#.#.#.#.......#.##
####.#####.###.###.#.#.#.#.###.#.###.###.#####.#.###.#.#.#.#.#.#.###.#.#.#####.#.#.#.###.#.###.#.#.#.#.#.######
##.#...#.....#.#.#.................#.#...#.........#.#.#.#...#.#.....#.#.#...#.#.#.#.........#.#...#.#.#...#.##
##.#.###.###.###.###.#.#.#.#####.###.###.#######.#####.#.#.###.#####.#.#.#.#.#.#.#.#.#.#####.#.#.#.#####.#.#.##
##.#.....#...#.....#.#.#.#.#...........#...#.......#.......#.....#...#.#.#.#...#.#.#.#.#...#.#.#.#.#.#...#...##
##.#.#######.#####.###.###.#########I#####J#####K###########G#####C#L#####B#############.#####.###.#.###.######
##.........#.#...#...#...#.#                                                       #...#.#.#...#.#.#.....#.#.##
####.#####.#####.#.#########                                                       #.###.#.###.#.#.###.#.#.#.##
##...#.#.#...#.#...#...#...#                                                       #.#...#.......#...#.#.....##
####.#.#.#####.###.###.#.###                                                       #.###.#####.#####.###.######
##...#.#.#.#...#...#.#...#.M                                                       #.#.#.#...#...#...#.#...#.##
##.#.#.#.#.###.###.#.###.#.#                                                       #.#.#.###.#.#####.#.#.###.##
##.#.......#.....#.....#...#                                                       #.#...#.#.........#.....#.##
##.###.#######.#.#.#.#.#.#.#                                                       #.#.#.#.#.#######.#.#.###.##
##.#...#.#...#.#.#.#.#.#.#.#                                                       N.#.#.#.#.......#...#...#.O#
##.#.###.#.###.###.#.###.###                                                       #.#.#.#.#.###.#########.#.##
#P.#...............#.......#                                                       #...#.#.....#.#.....#...#.Z#
######.#.#.#####.###.#.#####                                                       #.###.#.#########.###.###.##
#I.#.#.#.#...#.#...#.#.#...Q                                                       #.#.#...#.....#.#.#.......##
##.#.#########.#########.###                                                       ###.#####.#####.#.##########
##...#.#.#.#.....#.#.......#                                                       E...#...#.............#.#.##
##.#.#.#.#.###.#.#.###.#####                                                       ###.#.#####.###.#.#.###.#.##
##.#...........#.......#.#.#                                                       #.....#.....#...#.#...#...K#
##########.###.#########.#.#                                                       ###.###.###.#######.###.####
##.......#...#.#.#...#.....#                                                       #.#.....#.....#.....#.....##
##.#.###########.#.#.#.#.###                                                       #.###.#.###########.#####.##
##.#...#.#...#.#.#.#...#...#                                                       #.#.#.#.#.....#...........##
##.#.###.#.###.#.#.#####.###                                                       #.#.#####.###############.##
#R.#.......#.........#.....#                                                       #.............#.........#.##
######.###.###.#.#####.#####                                                       #.#.###.###.#######.#.#.####
##...#.#.......#...#.......O                                                       F.#.#.#.#.......#.#.#.#...##
##.#########.###.###.#.#.###                                                       #.###.#####.###.#.###.#.####
#N.......#.#...#.#...#.#.#.#                                                       #.#.....#...#.#.....#.#...##
##.#.#.#.#.#.#########.###.#                                                       #.###.#######.###.#.#.#.#.##
##.#.#.#.#.#.#.#...#...#.#.#                                                       #.#.....#.#...#.#.#...#.#.S#
######.#.#.###.#.#######.#.#                                                       #####.###.###.#.############
##.....#...#.#...#.#.......T                                                       U.........#...............V#
##########.#.###.#.###.#####                                                       #.#.###.###.#.#.###.###.#.##
##.#...#...................#                                                       #.#.#.....#.#.#...#.#...#.##
##.###.#####.#.#.#.#.#######                                                       #.#####.###.#########.######
##.........#.#.#.#.#.#.....#                                                       #.#.#.......#.#...#.#.#...##
####.#.#########.#####.###.#                                                       ###.#.#######.#.###.###.####
#U...#.#.......#.#.#.#.#...#                                                       #.....#.....#.............##
######.#.###.#####.#.#.#.###                                                       ###.#####.#.###.###.#.#.#.##
##.....#.#.....#.#...#.#...W                                                       #.#...#.#.#.....#.#.#.#.#.X#
##.#####.#.#####.#.#.#.#####                                                       #.#####.#.#.#.###.#.###.####
##.......#.........#.....#.#                                                       3.#.#.#...#.#.#.#.#.#.#...##
##################.###.#.#.#                                                       #.#.#.###.#.###.#.###.#.####
##...........#...#.#...#.#.#                                                       #.........#.#...#.#.....#.##
##.###.#.###.#.#.#.#.#####.#                                                       #.###.#########.#.#######.##
##.#.#.#...#...#.#.#.....#.0                                                       #.#...#.....#...#.....#...L#
##.#######.#.#############.#                                                       #######.#.#.#.#.#.#.#.#.####
#A.....#...#.......#.....#.#                                                       X...#...#.#...#...#.#.#...##
####.#######.###.#.###.###.#                                                       ###.#.#####.#####.###.#.#.##
#M.....#.#...#.#.#.........#                                                       #.#...#.........#.#.#.#.#.##
########.#####.###.#.###.###                                                       #.#.#######.###.###.#.###.##
#T.#.#...#.#.#.#.#.#.#.#.#.1                                                       #.....#.......#...#.......##
##.#.###.#.#.#.#.#####.###.#                                                       #.#######.#.###.#####.######
##.#.....#.....#...#.....#.#                                                       #.#.#.....#.#.....#.......##
##.#.#.###.###.#.#.#.###.#.#                                                       #.#.#.#.#.#.###.#####.#.####
##...#.....#.#...#...#.....#                                                       #.#.#.#.#.#.#.......#.#...##
######.#.#.#.#.#####.###.#.#                                                       ###.#####.#.###.#####.#.#.##
##.....#.#...#...#.....#.#.#                                                       #.#.#...#.#.#.#.#...#.#.#.##
####.#.###.#######.#.#####.#######R###H#########D#######P#######S#####2#######V#####.#.#.###.###.#.###.#.#.####
##...#.#.....#.....#.....#.......#...#.#.......#.#.#.....#...#.#.....#...#.....#...........#...#.......#.#...##
##.###.###.#.###.#.###.###.#.#####.###.#.###.###.#.###.###.#.#.###.###.#.#.#####.###.#.#.#######.#####.########
##.#.....#.#.#...#.#.....#.#.#.....#.....#...#...#.#.....#.#.#.......#.#.#.........#.#.#.#.#.......#.#.#.....##
######.#####.#####.###.#.#########.#######.#.#.###.#####.#.#.#.#########.###.###.#.#.#####.###.#####.#.###.####
##.........#.#.....#...#.#.#.........#.....#.#.......#.....#.#.......#...#...#...#.#.......#.#.#.............##
######.###.#######.#####.#.###.#####.#.#############.#######.#######.###.#.#####.#.###.###.#.#####.###.#####.##
##.......#.#...#.....#.......#.#.#.#.#.....#.#.......#.....#...#.#.....#.#.#...#.#.#.....#.....#...#.......#.##
##.###.#####.#.###.#.#.#.#######.#.#.###.###.#.#####.###.#.#.###.###.###.#.#.#########.#####.#.###.#.###.#.#.##
##.#.........#.#.#.#.#.#.#.....#.....#.......#.....#.#...#...#...#.#...#.#...........#.#.#.#.#...#.#.#.#.#.#.##
##.#.#.#.#.###.#.#############.###.#.#.#######.#####.#.###.###.###.#.###.#.#.#.#####.#.#.#.#.#########.#.######
##.#.#.#.#...#.#.#.....#...#.#...#.#.#...#.........#.#.#.......#.....#...#.#.#.#.....#...#.#.......#.#.....#.##
##.#.#.#.#.#####.###.#####.#.#.#.###.#.#.#.#######.#.#.###.#.#####.###.###.###############.#.#######.#####.#.##
##.#.#.#.#.....#.........#.#.#.#.#...#.#.#.#.....#.#.#.#...#.#.......#.#...#.#.......#...#.#...#...#.........##
####.#.#.###.###.#.#.###.#.#.###.#.###.#####.#.###.###.#############.#.###.#.#####.###.###.#.#####.#.#####.#.##
##...#.#.#.....#.#.#.#...............#.#...#.#.......#.........#.....#.#.#.......#.....#.#.........#.#.....#.##
########################.#.#########.#.###.#####.###.#.###########.#.#.#.###.#####.#####.###.#########.#.###.##
##.#.......#.............#.#.#.......#.......#.#.#.#.#...#...#.....#.#...#.....#.#.#...#.#.#...#.....#.#...#.##
##.#####.#.###.#############.#.###.#####.#####.###.#.#.###.#####.#######.#.#####.#.###.#.#.#######.#####.#.#.##
##.......#.......#.........#.#.#.....#.......#.......#.#.#.#.#.#.......#.#.....#...#.#...#.#.....#.#.#...#.#.##
##.###.###.#.#.###########.#.#######.#.#####.#######.#.#.#.#.#.#####.#.#.###.#.###.#.#.#.#.#.#.#.#.#.###.######
##...#.#...#.#.#.....#...#.....#...#.#.#.#...#...#...#.........#.....#.#.#...#...#.....#.....#.#.....#.#.....##
####.###.###.#####.#####.#.###.#.###.#.#.#######.###.#.#.###.#.#####.###.#.#######.#####.#######.#####.#####.##
##...#...#...#...........#.#.........#.......#.....#.#.#.#...#.#.#.....#.#...#.#.#.#.#...#.....#.#.......#.#.##
##.#.###.###.#####.#####.#####.#########.###.###.###.#####.#####.###.###.###.#.#.#.#.#####.#.###.#.#.#.#.#.####
##.#.#...#.......#...#.............#.......#.#.......#.........#.....#.....#...............#...#...#.#.#.....##
##################################W###2#########Q#######3#########0###1#######J################################
###############################################################################################################"

let testInput = "##################
########A#########
########.........#
########.#######.#
########.#######.#
########B#######.#
######       ###.#
#B..##       ###.#
###.##       ###.#
###..D       ###.#
######       ###.#
##########F#####.#
#D.#######...###.#
##.#########.###.#
#F.#########.....#
############Z#####
##################"

type Tile =
  | Empty
  | Wall
  | InnerPortal of char
  | OuterPortal of char
  | Start
  | Finish


let (|PortalChar|_|) (c: char) =
  if (c>='a' && c<='z') || (c>='B' && c<='Y') || (c>='0' && c<='9') then // exclude A and Z because that is start and finish
    Some c
  else
    None

let findPositions width (board: Tile array) tile =
  board
  |> Array.findIndex (fun x -> x = tile)
  |> (fun i -> (i%width, i/width))

let isPassable level tile =
  match tile with
  | Empty | InnerPortal _ -> true
  | Wall -> false
  | OuterPortal _ -> level > 0
  | Start | Finish -> level = 0
  

let searchPath width board =
  let (startX, startY) = findPositions width board Start
  let openNodes = Queue<int*int*int*int>() // level x y dist
  let visitedNodes = HashSet<int*int*int>() // level x y
  
  openNodes.Enqueue ((0, startX, startY, 0))

  let mutable found = false
  let mutable resultDistance = System.Int32.MaxValue
  let mutable counter = 0
  while not found do
    counter <- counter+1
    let (level, x,y,dist) = openNodes.Dequeue()
    visitedNodes.Add (level, x, y) |> ignore
    let tile = board.[x + y*width]
    // printfn "%A %A" (level, x, y, dist) tile
    match tile with
    | Empty | Start ->
      // add adjacent nodes
      [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|]
      |> Array.iter (fun (nx, ny) ->
        if isPassable level board.[nx + ny*width] && not (visitedNodes.Contains (level, nx, ny)) then
          openNodes.Enqueue((level, nx, ny, dist+1))
      )
    | InnerPortal p ->
      // find other portal
      let (opx, opy) = findPositions width board (OuterPortal p)
      [|(opx-1,opy); (opx+1,opy); (opx,opy-1); (opx,opy+1)|]
      |> Array.iter (fun (nx, ny) ->
        if isPassable (level+1) board.[nx + ny*width] && not (visitedNodes.Contains (level+1, nx, ny)) then
          openNodes.Enqueue((level+1, nx, ny, dist+2))
      )
    | OuterPortal p ->
      // find other portal
      let (opx, opy) = findPositions width board (InnerPortal p)
      [|(opx-1,opy); (opx+1,opy); (opx,opy-1); (opx,opy+1)|]
      |> Array.iter (fun (nx, ny) ->
        if isPassable (level-1) board.[nx + ny*width] && not (visitedNodes.Contains (level-1, nx, ny))  then
          openNodes.Enqueue((level-1, nx, ny, dist+2))
      )
    | Finish ->
      resultDistance <- dist
      found <- true
    | _ ->
      failwith (sprintf "Ended up on wall. should not happen: %d %d" x y)
  counter, resultDistance

[<EntryPoint>]
let main argv =
  let width = input.Split("\r\n").[0].Length
  let board =
    input.Split("\r\n")
    |> Array.mapi (fun y l ->
      l
      |> Seq.mapi (fun x t ->
        match t with
        | PortalChar c -> if x=1 || y=1 || x=109 || y=111 then OuterPortal c else InnerPortal c
        | ' ' | '#' -> Wall
        | '.' -> Empty
        | 'A' -> Start
        | 'Z' -> Finish
        | x -> failwith (sprintf "Unknown board character %A" x)
      )
      |> Seq.toArray
    )
    |> Array.collect id

  let timer = System.Diagnostics.Stopwatch.StartNew()
  let result = searchPath width board

  printfn "%A %dms" result timer.ElapsedMilliseconds
  0 // return an integer exit code