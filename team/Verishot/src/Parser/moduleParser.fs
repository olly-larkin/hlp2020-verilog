module Verishot.ModuleDefinition

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Expression
open Verishot.Token

/// Top level parser for modules
/// Parses the top level (module definition)
let rec ParseModuleDefinition inp =
    inp |> (Keyword.Module >=> Identifier >=> ListOfPortsParser >=> Symbol.Semmicolon ?=> ModuleItemListParser >=> Keyword.Endmodule <&> fun (((((_,a),b),_),c),_) -> 
        match c with
        | None -> { name = a; ports = b; items = [] }
        | Some c -> { name = a; ports = b; items = c })

/// Parses the list of ports in brackets
and ListOfPortsParser inp =
    inp |> (Symbol.LeftRoundBra ?=> PortListParser >=> Symbol.RightRoundBra <&> fun ((_,a),_) ->
        match a with
        | None -> []
        | Some (a) -> a)

/// Parses list of ports
and PortListParser inp =
    inp |> (Identifier ?=> (Symbol.Comma >=> PortListParser) <&> fun (a, b) ->
        match b with
        | None -> [a]
        | Some (_, lst) -> a::lst)

and ModuleItemListParser inp =
    inp |> (ModuleItemParser ?=> ModuleItemListParser <&> fun (a,b) ->
        match b with
        | None -> [a]
        | Some lst -> a::lst)

and ModuleItemParser inp =
    inp |> (buildParser [ 
            PortDeclarationParser 
            AssignParser 
            WireDeclarationParser
            ItemInstantiationParser
        ] >=> Symbol.Semmicolon <&> fun (a,_) -> a)

and RangeParser inp =
    inp 
    |> (Symbol.LeftSquareBra >=> Number.Value ?=> (Symbol.Colon >=> Number.Value) >=> Symbol.RightSquareBra)
    |> function
        | Ok ((((_,a),None),_), rest, opErr) -> Ok (Range (a, a), rest, opErr)
        | Ok ((((_,a),Some (_, b)),_), rest, opErr) when a >= b -> Ok (Range (a, b), rest, opErr)
        | Ok ((((_,a),Some (_, b)),_), _, _) -> Error (sprintf "The first element (%A) of the range is smaller than the second element (%A)." a b, inp)
        | Error err -> Error err

and PortDeclarationParser inp =
    let portMap (dir: Direction) =
        fun ((_,range),a) ->
            match range with
            | None -> ItemPort (dir, Single, a)
            | Some range' -> ItemPort (dir, range', a) 
    inp |> buildParser [ 
        Keyword.Input ?=> RangeParser >=> Identifier <&> (portMap Input)
        Keyword.Output ?=> RangeParser >=> Identifier <&> (portMap Output)
    ]

and AssignParser inp =
    inp |> (Keyword.Assign >=> Identifier >=> Symbol.AssignEqual >=> ExpressionParser <&> fun (((_,a),_),b) -> ItemAssign (a,b))

and WireDeclarationParser inp =
    inp |> (Keyword.Wire ?=> RangeParser >=> Identifier <&> fun ((_,range),iden) ->
        match range with
        | None -> ItemWireDecl (Single, iden)
        | Some range' -> ItemWireDecl (range', iden))

and ItemInstantiationParser inp =
    inp |> (Identifier >=> Identifier >=> Symbol.LeftRoundBra ?=> ExpressionListParser >=> Symbol.RightRoundBra <&> fun ((((a,b),_),c),_) ->
        match c with
        | None -> ItemInstantiation (a, b, [])
        | Some lst -> ItemInstantiation (a, b, lst))