module day18try2
open System.Collections.Generic
open Priority_Queue;

let input = "#################################################################################
#.......#.A...........#.........#.......#.#.........#.............#...#..r......#
#.###.###.#####.#####.#.#######.#.#######.#.#.#####.#.#.#.#######.###.#.###.###.#
#...#.#...#.....#.#...#...#.....#.......#...#.#...#o#.#.#.#...#.#...#.#...#.#...#
#####.#.###.#####.#.###.#W###.#########.#####.#.#.###.#.#.#.#.#.###.#K###.#.#####
#.....#.#.#...#...#...#.#...#...........#...#...#.....#.#.#.#.#.#...#...#.#.....#
#M###.#.#.###.#.#####.#####.###########.#.#.###.#######.###.#.#.#.#####.#.#####.#
#.#...#...#...#.#...#.....#...#..z....#.#.#...#...#.....#...#.#.#.#...#.......#.#
#.###.###.#.###.#.#.#####.###.#.###.#.#.#.###.#####.#.###.###.#.#.#.#.#####.###.#
#...#...#.#.#....y#.....#.....#.#.#.#.#.#...#.#...#.#.#...#...#.#...#.#...#.#m..#
#.#.#####.#.###########.#######.#.#.#.#.###.#.#.#X#.#.#.###.###.#####.#.#.###.###
#.#.#...#.#s..#.......#.......#...#.#.#.#...#...#.#.#.#.#.#.#.......#...#...#...#
###.#.#.#.###.#.#####.###.###.#####.#.#.#.#######.#.###.#.#.#.#####.#######.###.#
#...#.#...#.#...#...#.#...#.......#.#.#.#.#.......#.....#.#.#.#.....#.....#.....#
#.###.#####.#####.#.#C#.###.#####.#.#.#.#.#####.###.#####.#.###.#####.#.#.#####.#
#.....#.....#.....#.#.#.#...#...#...#.#.#.#.F.#...#...#...#...#.#...#.#.#.....#.#
#.#######.#.###.###.#.#.#####.#.#######.#.#.#.###.###.###.###.#.#.#.#.#.#######.#
#.#.......#...#.#...#.#...#...#.#.......#...#.#.#...#.......#.#.#.#.#.#.........#
#.#.#########.#.#####.###N#.###.#.###########.#.###.#########.#.#.#.#.###########
#...#...#.......#.....#...#.#.#.#.......#...#.#...............#...#.#.#....q....#
#####.###.#######.#####.###.#.#.#######.#.#.#.#############.#####.#.#.#.#.#####.#
#g..#.....#.....#...#.#...#...#.......#.#.#.#..l..........#.#...#.#.#.#.#...#...#
###.#.#####.###.###.#T#.#.###.###.#####.###.#############.#####B#.#.#.#.###.#####
#...#.#...#...#.....#.#.#...#.#.#.#...#.#.......#.......#...#...#.#...#...#.....#
#.###.###.###.#####.#.###.###.#.#.#.#.#.#.#####.#.###.#####.#.###.#####.#######.#
#...#...#...#.#.#...#...#.I...#.#...#...#.....#.#...#.#...#...#.#...#.#.#.......#
###.###.#.###.#.#.#####.#####.#.#############.#####.#.#.#.#####.###.#.#.#.#######
#.....#.#...#.#.......#.#...#...........#.....#.....#...#.......#.#.#.#.#.......#
#.#####.###.#.#######.#.#.#.#####.#######.###.#.###########.###.#.#.#.#.#######.#
#...........#.#e....#.#...#...#...#.....#.#...#...#.........#.....#.#...#.......#
#.###########.#.###.#########.#####.###.#.#######.#.#####.#######.#.#####.#####.#
#.....#.#.....#.#.#.........#.......#...#.#.......#.#.....#.....#.#...#...#.#...#
#####.#.#.#####.#.#####.#.###########.###.#.#######.#####.#.###.#.###.#.###.#.###
#...#.#.#.......#.#.....#.#.........#.#.#...#.#.....#...#.#.#...#...#.#...#...#.#
#.###.#.#########.#.#####.#V#.#####.#.#.#.###.#.###.#.#.###.#.#######.###.#.###.#
#.#...#.....#.....#.#.....#.#...#...#.#.#.#...#.#.#.#.#.....#.........#...#.#...#
#.#.###.###.#.#####.#######.###.#.###.#.#.###.#.#.#.#.#################.###.#.#.#
#.#.....#.#.#.....#.......#.#.#.#...#.#.#.#...#.#...#.#.........#.......#.S.#.#.#
#.#######.#.#####.#######.#.#.#.#####.#.#.#.#.#.#####.#####.###.#.#######.###.#.#
#.......................#.....#........@#@..#.#.............#.....#...........#.#
#################################################################################
#...............#.....#.....#..........@#@#.........................D.......#..u#
#.#############.#####.#.#.###.###.#####.#.#.#.#####.#######.###############.#.###
#.....#.....#...#...#...#.....#.#.#.....#...#....v#.#.....#.#.......#.....#.#.E.#
#####.#.#####.###.#.#.#########.#.#######.#######.###.###.###.#####.#.#####.#.#.#
#.....#.....#.....#.#.#...#.....#.......#.#.......#.....#.#...#...#.#....j#t#.#.#
#.#########.#######.#.#.#.#.###########.#.#.#######.#####.#.###.###.#####.#.#.#.#
#.......#...#...#...#...#...#.......#...#.#.#.....#.#.....#.#.....#.#...#.#.#.#.#
#######.#.#.###.#.#######.###.#####.#.#####.#.###.#.#.#####.#####.#.#.#.#.#.###.#
#.....#.#.#.....#...#.....#...#.....#...#...#.#.#...#.....#.......#...#.#.#.#...#
###.###.#.#####.###.#####.#.###.#####.#.#.#.#.#.#########.#######.#####.#.#.#.#.#
#...#...#...#...#.#.....#.#.#.#.#.....#.#.#d#.#.....#.......#.....#.....#.#...#.#
#.###.#####.#.###.#####.#.#.#.#.#######.#.###.###.#.#.#######.#####.#####.#####.#
#.....#...#.#.....#.....#.#.#.#...#.....#.....#...#.#.....#.....#...#...#.....#.#
#.#####.#.#.#.#####.#####.#.#.###.#.###.#######.#.#######.#.#####.#####.#.#.#.#.#
#.#.....#.#.#.#.....#.....#.#...#.#.#...#.......#.#.....#...#.#...#.....#.#.#.#.#
#.###.#.#.#.#.#.###########.###.#.#.#.###.#########.#.#######.#.#####.###.#.###.#
#.H.#.#.#.#.#.#.....#.....#.#...#...#...#.#.....#...#.........#.#.....#...#.....#
###.###.#.#.#.#####.#.###.#.#.#########.#.#.###.#.#######.#####.#.#.#.#.#########
#.#.#...#...#...#.#...#...#.#...#.......#...#.#...#.....#...#...#.#.#.#.........#
#.#.#.#.#######.#.#####.###.#.#.#.#######.###.#########.###.#.#####.#.#########.#
#...#.#.#...#.#.#...#...#...#.#.#...#...#...#...#.....#...#.#.#.....#.#.......#.#
#.###.###.#.#.#.###.#######.#.#.###.#.#.###.#.###.#.#.#.#.#.###.#####.###.###.#.#
#.#...#...#.#.#.#...........#.#...#.#.#.#.#.#.#...#.#...#.#.....#...#...#.#.#.#.#
#.###.#.###.#.#.#.###########.###.#.#.#.#.#.#.#.###.#####.#####.###.###.#.#.#.#.#
#...#...#.#.#.#...#...#.U...#.#.#...#.#.#...#.....#.#.....#...#...#...#...#...#.#
###.#.###.#.#.#####J#.#.#.###.#.#####.###.#######.#.#####.#.#####.#.#####.#####.#
#...#.Q.#.#.#.....#.#...#.....#.........#.......#.#f....#.L.#.....#h....#.#...#.#
#.#####.#.#.###.#.#.#################.#.#######.#.#####.###.#.#####.#.###.#.#.#.#
#.#...#...#...#.#.#.#...#.......#...#.#.#.#...G.#.....#.Z.#.#...#...#.....#.#...#
#.#.#####.###.###.#.#.#.#####.#.#.#.###.#.#.#############.#.###.#########.#.#####
#k#.#.....#.#...#.#..c#...#...#...#.#...#.#.#........n#...#...#.#.......#.#...#.#
#.#.#.#####.###.#.#######.#.#######.#.#.#.#.#.#######.#.###.###.#.#####.#####.#.#
#.#...#.....#...#.#.P...#.#.#.....#...#.#.#.#.#...#...#.#...#...#.....#.....#p..#
#.###.#.#.###.###.#.#.###.#.###.#.#####.#.#.#.#.###.###.###.#.#######.###.#####.#
#...#...#.#...#.#...#...#.#...#.#...#...#.O.#.#.....#.#...#b#.#.......#.#.#...#.#
###.#####.#.###.#.#####.#.###.#####.#.###.###.#.#####.#.#.###.#.#######.#.#.#.#.#
#.#.#.....#.#a..#.#...#.#.#...#.....#...#...#i#.#...#...#.Y.#.#.#....w..#.#.#...#
#.#R#######.#.#.#.###.#.#.#.###.#######.###.#.#.#.#.#######.#.#.#####.#.#.#.#####
#...........#.#.......#.....#...........#.....#x..#.........#.........#.#.......#
#################################################################################"

let testInput = "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"

type Tile =
  | Wall
  | Empty
  | Door of char
  | Key of char
  | Start

let (|MatchKey|_|) (c: char) =
  if c >= 'a' && c <= 'z' then
    Some (c |> System.Char.ToUpper)
  else
    None

let (|MatchDoor|_|) (c: char) =
  if c >= 'A' && c <= 'Z' then
    Some c
  else
    None

type Move = {
  x: int
  y: int
  distance: int
  t: Tile
}

let findMoves width (board: Tile array) (x,y) (keys: HashSet<char>) =
  let openNodes = Queue<int*int*int>()
  openNodes.Enqueue (x,y,0)
  let results = ResizeArray<Move>()
  let visistedNodes = HashSet<int*int>()
  while openNodes.Count > 0 do
    let (x,y,d) = openNodes.Dequeue()
    visistedNodes.Add (x,y) |> ignore
    let canMove =
      match board.[x + y * width] with
      | Key k ->
        results.Add ({x = x; y = y; distance = d; t = Key k})
        true
      | Door k ->
        if keys.Contains k then
          results.Add ({x = x; y = y; distance = d; t = Door k})
        false
      | _ -> true

    if canMove then
      [(x-1, y); (x+1,y); (x,y-1); (x,y+1)]
      |> List.iter (fun (nx,ny) ->
        if not <| visistedNodes.Contains (nx,ny) && not <| openNodes.Contains (nx,ny,d+1) then
          match board.[nx + ny * width] with
          | Empty | Key _ | Door _ ->
            openNodes.Enqueue (nx, ny, d+1)
          | _ -> ()
        )
  results

let findOtherKeys width (board: Tile array) (x,y) =
  let openNodes = Queue<int*int*int*int>()
  openNodes.Enqueue (x, y , 0 , 0)
  let results = ResizeArray<int*int*int>()
  let visistedNodes = HashSet<int*int>()
  while openNodes.Count > 0 do
    let (x,y,d, ds) = openNodes.Dequeue()
    visistedNodes.Add (x,y) |> ignore
    match board.[x + y * width] with
    | Key k ->
      results.Add (pown 2 (int k - int 'A'), d, ds)
    | _ -> ()

    [(x-1, y); (x+1,y); (x,y-1); (x,y+1)]
    |> List.iter (fun (nx,ny) ->
      if not <| visistedNodes.Contains (nx,ny) 
      && not <| openNodes.Contains (nx,ny,d+1, ds) 
      then
        match board.[nx + ny * width] with
        | Empty | Key _ ->
          openNodes.Enqueue (nx, ny, d+1, ds)
        | Door door ->
          openNodes.Enqueue (nx, ny, d+1, (pown 2 (int door - int 'A')) ||| ds)
        | _ -> ()
      )
  results |> Seq.toList

let rec backtrack allKeys currentPosKey currentKeys =
  if currentKeys |> List.length = 27 then
    0
  else
    let moves =
      allKeys
      |> Map.find currentPosKey
      |> Seq.filter (fun (ok, dist, doors) ->
        not (currentKeys |> List.contains ok) &&
        doors
        |> Seq.forall (fun d -> currentKeys |> List.contains d)
      )
    
    moves
    |> Seq.map (fun (ok, dist, doors) ->
      dist + (backtrack allKeys ok (ok::currentKeys))
    )
    |> Seq.min

type SearchState = {
  currentDist: int
  currentKeys: int
  currentPosKeys: int list
}

let containsAll l1 l2 =
  (l1 |> List.length) = (l2 |> List.length)
  && l1 |> List.forall (fun x -> l2 |> List.contains x)

let createHash (l1: char list) =
  l1
  |> List.tail
  |> List.fold
    (fun s x ->
      s ||| (int x - int 'A' + 1)
    )
    0
  |> (fun x -> x ^^^ (int l1.Head))

let bfs (allKeys: Map<int, (int * int * int) list>) (startKeys: int list) =
  let openNodes = SimplePriorityQueue<SearchState>()
  openNodes.Enqueue({currentKeys = 0; currentPosKeys = startKeys; currentDist = 0}, 0.f)

  let seenStates = Dictionary<int list *int, int>()

  let mutable result = None
  while result = None do
    let node = openNodes.Dequeue()
    // counter <- counter+1
    // if (counter % 100000 = 0) then printfn "%A" node

    let s,st = seenStates.TryGetValue((node.currentPosKeys, node.currentKeys))
    if not s || node.currentDist < st then
      seenStates.[(node.currentPosKeys, node.currentKeys)] <- node.currentDist
      let moves =
        node.currentPosKeys
        |> List.collect (fun p ->
          allKeys
          |> Map.find p
          |> List.filter (fun (ok, dist, doors) ->
            (node.currentKeys &&& ok <> ok) &&
            (doors &&& node.currentKeys) = doors
          )
          |> List.map (fun x -> (p, x))
        )
        
      moves
      |> List.iter (fun (p, (ok, dist, doors)) ->
        let nk = ok ||| node.currentKeys
        let ns = {currentKeys = nk; currentPosKeys = node.currentPosKeys |> List.map (fun x -> if x = p then ok else x); currentDist = dist + node.currentDist}
        if nk = 0x3FFFFFF then
          result <- Some (ns)
        openNodes.Enqueue(ns, float32 <| dist + node.currentDist)
      )
  result

[<EntryPoint>]
let main argv =
  let width = 81
  let height = 81
  let lastKey = 'Z'
  // let width = 24
  // let height = 6
  // let lastKey = 'I'

  let board = Array.create (width*height) Empty

  input.Split("\r\n")
  |> (fun inp ->
    for y in 0 .. height-1 do
      let line = inp.[y]
      for x in 0 .. width-1 do
        board.[y*width + x] <- 
          match line.[x] with
          | '#' -> Wall
          | '.' -> Empty
          | MatchKey k -> Key k
          | MatchDoor d -> Door d
          | '@' -> Start
          | x -> failwith (sprintf "Unknown character on board: %A" x)
  )

  let startPositions =
    board
    |> Array.indexed
    |> Array.filter (fun (_,t) -> t = Start)
    |> Array.map (fun (i,_) -> 
      (i%width, i/width)
    )

  startPositions
  |> Array.iter (fun (x,y) -> board.[x + y * width] <- Empty)
  

  let allKeys =
    ['A' .. lastKey]
    |> List.map (fun k ->
      let otherKeys =
        board
        |> Array.findIndex (fun t -> t = Key k)
        |> (fun i -> 
          (i%width, i/width)
        )
        |> findOtherKeys width board
      (pown 2 (int k - int 'A'), otherKeys)
    )
    |> Map.ofList
    |> (fun m ->
      startPositions
      |> Array.indexed
      |> Array.fold
        (fun m (i, sp) ->
          m |> Map.add ((pown 2 i)<<<27) (findOtherKeys width board sp)
        )
        m
    )

  let timer = System.Diagnostics.Stopwatch.StartNew()
  bfs allKeys (startPositions |> Array.indexed |> Array.map (fun (i,_) -> (pown 2 i)<<<27) |> Array.toList)
  |> printfn "%A"
  printfn "%dms" timer.ElapsedMilliseconds


  0 // return an integer exit code
