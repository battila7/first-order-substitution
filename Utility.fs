module Utility

let flattenResults (lst: List<Result<'a, 'b>>) =
    let foldr acc elem =
        elem |> Result.bind (fun inner -> acc |> Result.map (fun lst -> inner::lst))

    lst |> List.fold foldr (Ok [])
