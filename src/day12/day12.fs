module day12
open System.Text.RegularExpressions

let input = "<x=-10, y=-13, z=7>
<x=1, y=2, z=1>
<x=-15, y=-3, z=13>
<x=3, y=7, z=-4>"

let parseRegex = Regex """^<x=(-?\d+), y=(-?\d+), z=(-?\d+)>$"""

[<EntryPoint>]
let main argv =
  let moons =
    input.Split("\r\n")
    |> Array.map (fun x ->
      let r = parseRegex.Match x
      let x = r.Groups.[1].ToString() |> int
      let y = r.Groups.[2].ToString() |> int
      let z = r.Groups.[3].ToString() |> int
      (x,y,z, 0,0,0)
    )

  seq { 1..1000 }
  |> Seq.fold
    (fun s _ ->
      let nm =
        s
        |> Array.map (fun m ->
          s
          |> Array.fold
            (fun (x,y,z,dx,dy,dz) (ox,oy,oz,_,_,_) ->
              (x,y,z,dx + (sign (ox-x)),dy + (sign (oy-y)),dz + (sign (oz-z)))
            )
            m
          |> fun (x,y,z,dx,dy,dz) -> (x+dx,y+dy,z+dz,dx,dy,dz)
        )
      printfn "%A" nm
      nm
    )
    moons
  |> Array.sumBy (fun (x,y,z,dx,dy,dz) -> (abs x + abs y + abs z) * (abs dx + abs dy + abs dz))
  |> printfn "%A"
  0 // return an integer exit code
