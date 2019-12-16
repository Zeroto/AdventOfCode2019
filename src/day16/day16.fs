module day16

let input = "59769638638635227792873839600619296161830243411826562620803755357641409702942441381982799297881659288888243793321154293102743325904757198668820213885307612900972273311499185929901117664387559657706110034992786489002400852438961738219627639830515185618184324995881914532256988843436511730932141380017180796681870256240757580454505096230610520430997536145341074585637105456401238209187118397046373589766408080120984817035699228422366952628344235542849850709181363703172334788744537357607446322903743644673800140770982283290068502972397970799328249132774293609700245065522290562319955768092155530250003587007804302344866598232236645453817273744027537630"
let testInput = "12345678"

let pattern = [|0; 1; 0; -1|]

let getRepeatedPattern stride =
  // let basePattern =
  //   pattern
  //   |> Array.collect (fun x -> Array.create stride x)
  Seq.initInfinite (fun index ->
    let patternIndex = ((index+1) / stride) % 4
    pattern.[patternIndex]
  )

[<EntryPoint>]
let main argv =
  let timer = System.Diagnostics.Stopwatch.StartNew()

  let skip = 
    input.Substring(0,7)
    |> int

  let digits =
    input
    |> Seq.map (string >> System.Int32.Parse)
    |> Seq.replicate 10000
    |> Seq.collect id
    |> Seq.skip skip
    |> Seq.toArray
  
  printfn "Finished building input: %Ams" timer.ElapsedMilliseconds

  // let patterns =
  //   digits
  //   // |> Array.take 8
  //   |> Array.map (fun (i, _) ->
  //     getRepeatedPattern (i+1)
  //     |> Seq.take digits.Length
  //     |> Seq.toArray
  //   )

  //printfn "%s" (patterns |> Array.map (fun x -> x |> Array.map (sprintf "%2d") |> String.concat "") |> String.concat "\n")

  // printfn "Finished building patterns: %Ams" timer.ElapsedMilliseconds

  let length = digits |> Array.length
  let mutable prevIter = timer.ElapsedMilliseconds
  [1..100]
  |> List.fold
    (fun (s: int array) iter ->
      for i in length-2 .. -1 .. 0 do
        s.[i] <- (s.[i+1] + s.[i]) % 10

      // let output = Array.zeroCreate length
      // for i in 0 .. length-1 do
      //   let value =
      //     let mutable sum = 0
      //     for si in i..length-1 do
      //       let patternIndex = ((si+1) / (i+1)) % 4
      //       let m = pattern.[patternIndex]
      //       sum <- sum + (s.[si]*m)
      //     //printfn "%A %A %A" i (pattern |> Seq.take 8 |> Seq.map string |> String.concat ",") value
      //     (abs sum) % 10
      //   output.[i] <- value
      // printfn "%3d %s" iter (output |> Array.map (fun (_, v) -> sprintf "%2d" v) |> String.concat "")
      printfn "finished iter %A: %dms" iter (timer.ElapsedMilliseconds - prevIter)
      prevIter <- timer.ElapsedMilliseconds
      s
    )
    digits
  |> Seq.take 8
  |> Seq.map (string)
  |> String.concat ""
  |> printfn "%A"

  0 // return an integer exit code