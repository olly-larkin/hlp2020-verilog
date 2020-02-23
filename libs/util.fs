module Verishot.Util

module List =
    let catOptions (xs: 'a option list): 'a list =
        xs
        |> List.collect (function
            | Some item -> [ item ]
            | None -> [])
