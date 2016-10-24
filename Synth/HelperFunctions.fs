module Synth.HelperFunctions.List

let rec mapFirst mappingChooser list =
    match list with
    | [] -> []
    | x::xs ->
        match mappingChooser x with
        | Some(x') -> x'::xs
        | None -> x::mapFirst mappingChooser xs