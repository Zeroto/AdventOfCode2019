module day4

let hasDouble (n: string) =
  n 
  |> Seq.pairwise
  |> Seq.exists (fun (a,b) -> a = b)

let hasOnlyDouble (n: string) =
  n
  |> Seq.fold 
    (fun (prev, l, hasDouble) x ->
      match prev with
      | None -> (Some x, 1, hasDouble)
      | Some p -> 
        if p <> x && l = 2 then
          (Some x, 1, true)
        else if p <> x then
          (Some x, 1, hasDouble)
        else
          (Some x, l+1, hasDouble)
    )
    (None, 0, false)
  |> (fun (_,l,d) ->  l = 2 || d)

let isAscending (n: string) =
  n
  |> Seq.pairwise
  |> Seq.forall (fun (a,b) -> b >= a)

[<EntryPoint>]
let main argv =
    let inputs = seq { 165432 .. 707912 }
    inputs
    |> Seq.filter (fun x -> 
      let s = string x
      isAscending s && hasOnlyDouble s
    )
    |> Seq.length
    |> printfn "%A"

    // printfn "%A" (hasOnlyDouble "123444")
    0 // return an integer exit code
