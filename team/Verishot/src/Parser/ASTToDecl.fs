module Verishot.ASTToDecl

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST

let ASTToDecl (inp: Module): ModuleDecl =
    let ports = 
        inp.items
        |> List.choose
            (function
            | ItemPort (a, b, c) -> Some (a, c, b)
            | _ -> None)

    { name=inp.name; ports=ports }