module day10

let input = "..#..###....#####....###........#
.##.##...#.#.......#......##....#
#..#..##.#..###...##....#......##
..####...#..##...####.#.......#.#
...#.#.....##...#.####.#.###.#..#
#..#..##.#.#.####.#.###.#.##.....
#.##...##.....##.#......#.....##.
.#..##.##.#..#....#...#...#...##.
.#..#.....###.#..##.###.##.......
.##...#..#####.#.#......####.....
..##.#.#.#.###..#...#.#..##.#....
.....#....#....##.####....#......
.#..##.#.........#..#......###..#
#.##....#.#..#.#....#.###...#....
.##...##..#.#.#...###..#.#.#..###
.#..##..##...##...#.#.#...#..#.#.
.#..#..##.##...###.##.#......#...
...#.....###.....#....#..#....#..
.#...###..#......#.##.#...#.####.
....#.##...##.#...#........#.#...
..#.##....#..#.......##.##.....#.
.#.#....###.#.#.#.#.#............
#....####.##....#..###.##.#.#..#.
......##....#.#.#...#...#..#.....
...#.#..####.##.#.........###..##
.......#....#.##.......#.#.###...
...#..#.#.........#...###......#.
.#.##.#.#.#.#........#.#.##..#...
.......#.##.#...........#..#.#...
.####....##..#..##.#.##.##..##...
.#.#..###.#..#...#....#.###.#..#.
............#...#...#.......#.#..
.........###.#.....#..##..#.##..."

let testInput = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"

[<EntryPoint>]
let main argv =
  let trees =
    input.Split("\r\n")
    |> Array.indexed
    |> Array.collect (fun (y, line) ->
      line
      |> Seq.indexed
      |> Seq.collect (fun (x, c) ->
        if c = '#' then
          [(x,y)]
        else
          []
      )
      |> Seq.toArray
    )

  let bestMonitoringPos =
    trees
    |> Array.map (fun (t1x, t1y) ->
      trees
      |> Array.map (fun (t2x, t2y) ->
        let (dx, dy) = (t2x - t1x, t2y - t1y)
        let t = atan (float dy / float dx)
        if (dx < 0) then
          t + System.Math.PI
        else
          t
      )
      |> Array.distinct
      |> (fun x -> (t1x, t1y, Array.length x) )
    )
    |> Array.maxBy (fun (_,_,l) -> l)
  printfn "%A" bestMonitoringPos

  let (lx, ly, _) = bestMonitoringPos
  trees
  |> Array.filter (fun (tx, ty) -> tx <> lx || ty <> ly) // remove self from list
  |> Array.map (fun (tx, ty) ->
    let (dx, dy) = (tx - lx, ty - ly)
    let t = -atan (float dx / float dy)
    let angle = // adjust angle for trees that have the same gradient but are on the other side
      if (dy < 0) then
        t
      else
        t + System.Math.PI
    let angle = // make sure we are in the positive range only
      if angle < 0. then
        angle + (System.Math.PI*2.)
      else
        angle
    let distSqr = dx * dx + dy * dy
    (angle, distSqr, (tx, ty))
  )
  |> Array.sortWith (fun (aa, ad, _) (ba, bd, _) -> // initial sort so we know which trees are on the same angle and are closest
    let t = aa - ba
    if t = 0. then
      ad - bd
    else
      sign t
  )
  |> Array.fold
    (fun m (a, _, p) ->
      let rec calcNewAngle na = // a tree that is on the same angle but further away can be thought of having an extra 2PI angle
        if Map.containsKey na m then
          calcNewAngle (na + System.Math.PI*2.)
        else
          na
      m |> Map.add (calcNewAngle a) p
    )
    Map.empty
  |> Map.toArray
  |> Array.sort // final sort by (adjusted) angle. This is probably not needed because of how the hash of floats is calculated, but it can't hurt
  |> (fun x -> x.[199])
  |> printfn "%A"
  0 // return an integer exit code
