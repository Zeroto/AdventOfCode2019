module day14

open System.Collections.Generic

let input = "1 BNZK => 2 NMDF
3 KPQPD => 4 GSRWZ
2 ZRSFC => 7 SRGL
5 XNPDM, 1 FGCV => 7 HMTC
18 LHTNC, 1 WGXGV => 9 CDKF
24 BMQM => 5 FKHRJ
2 LFPNB => 6 XNSVC
9 ZKFRH, 4 XGPLN, 17 SPQP, 2 GVNTZ, 1 JMSCN, 9 SHQN, 1 DZLWC, 18 MSKQ => 7 TXDQK
2 QFTW => 9 JPZT
1 KJCK, 1 TFKZ, 2 XNSVC => 7 GQRB
16 JPZT, 3 DCPW => 7 KJCK
24 LGKPJ, 11 CDKF, 2 HVZQM => 7 RNXJ
1 NMDF, 16 DBLGK, 1 HVZQM => 7 ZKFRH
4 TXDQK, 55 TNZT, 39 KDTG, 6 NVBH, 15 SDVMB, 53 XVKHV, 28 FKHRJ => 1 FUEL
3 CDKV, 11 FGCV => 1 NVBH
3 SPNRW, 7 JMSCN => 9 XMCNV
14 FGCV, 3 CQLRM, 1 TFKZ => 6 PQVBV
5 KJCK, 10 DCPW => 7 DSKH
5 NMDF, 1 TFKZ => 5 DZLWC
1 TNZT => 6 RTSBT
178 ORE => 6 XVLBX
1 SPNRW => 5 CWKH
15 ZRSFC, 2 PQVBV, 2 SRGL => 3 SPNRW
1 SHQN, 7 XNSVC => 4 QWMZQ
5 NVBH, 41 SHQN => 4 BNZK
1 CDKV, 6 KJCK => 4 TNZT
5 ZTBG, 1 HVZQM, 27 CDKV, 1 LHTNC, 2 RTSBT, 2 SHQN, 26 DZLWC => 9 KDTG
11 CDKV => 7 SHQN
13 QWMZQ, 19 FCFG => 7 GVNTZ
1 SHQN, 4 XNSVC => 9 ZRSFC
2 ZKFRH, 9 HVZQM, 1 KJCK, 3 GQRB, 11 DBLGK, 8 DZLWC, 2 SPQP, 5 RNXJ => 8 SDVMB
5 SPNRW => 7 JMSCN
2 XVLBX, 19 KPQPD => 7 XNPDM
2 JPZT => 8 CDKV
1 GQRB => 7 MSKQ
1 SHQN, 13 DSKH => 3 MHQVS
9 JPZT => 8 LFPNB
15 SPNRW, 4 GQRB => 9 SPQP
1 JPZT => 3 TFKZ
1 BMQM => 6 FGCV
24 FKHRJ => 9 DCPW
2 GSRWZ => 8 XGPLN
5 QPSDR, 1 XVLBX => 6 BMQM
128 ORE => 7 QPSDR
2 LHTNC, 6 FCFG, 5 GVNTZ => 7 ZTBG
9 KJCK, 6 MHQVS, 5 NVBH => 6 KRDGK
3 HMTC, 4 QWMZQ => 2 FCFG
4 WGXGV, 5 PQVBV => 1 LGKPJ
42 XVLBX => 5 CQLRM
1 CWKH => 9 DBLGK
1 KRDGK, 2 GQRB, 12 TFKZ => 5 LHTNC
1 CQLRM, 1 HMTC => 8 WGXGV
116 ORE => 1 QFTW
13 XMCNV => 5 XVKHV
12 LGKPJ, 8 FKHRJ => 9 HVZQM
5 QPSDR => 6 KPQPD"

let testInput = "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"

let testInput2 = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

let parseMaterial (s: string) =
  let s2 = s.Trim().Split(" ")
  (int64 s2.[0], s2.[1])

let rec findMaterialCount materialNeeded totalNeeded (wasteMaterials: Dictionary<string, int64>) (recipes: Map<string, int64 * (int64 * string) array>) (result: Dictionary<string, int64>) =
  //printfn "%A %A %A %A" materialNeeded totalNeeded wasteMaterials result
  let recipe = recipes |> Map.find materialNeeded

  let s,previousWaste = wasteMaterials.TryGetValue materialNeeded
  let totalNeeded = 
    if s then
      if (totalNeeded >= previousWaste) then
        wasteMaterials.[materialNeeded] <- 0L
        totalNeeded - previousWaste
      else
        wasteMaterials.[materialNeeded] <- previousWaste - totalNeeded
        0L
    else
      totalNeeded

  if totalNeeded > 0L then
    let iterationCount = int64 (ceil <| ((float totalNeeded) / (float <| fst recipe)))
    let neededMaterials = snd recipe |> Array.map (fun (t, r) -> (t * iterationCount, r))
   
    let wasted = fst recipe * iterationCount - totalNeeded
    wasteMaterials.[materialNeeded] <- wasted
    neededMaterials
    |> Array.iter
      (fun (c, r) ->
        if r = "ORE" then
          result.[r] <- result.[r] + (int64 c)
        else
          findMaterialCount r c wasteMaterials recipes result
      )

[<EntryPoint>]
let main argv =
  let recipes =
    input.Split("\r\n")
    |> Array.fold
      (fun s x ->
        let sp = x.Split("=>")
        let result = parseMaterial sp.[1]
        let materials =
          sp.[0].Split(", ")
          |> Array.map parseMaterial
        s
        |> Map.add (snd result) (fst result, materials)
      )
      Map.empty

  let result = Dictionary<string, int64>()
  result.["ORE"] <- 0L
  let waste = Dictionary<string, int64>()

  findMaterialCount "FUEL" 7659732L waste recipes result // manual binary search for part 2

  printfn "%A" result
  0 // return an integer exit code
