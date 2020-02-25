module Verishot.ModuleDefinition

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Expression
open Verishot.Token

let rec ParseModuleDefinition inp =
    inp |> (Keyword.Module >=> Identifier >=> ListOfPortsParser >=> Symbol.Semmicolon ?=> ModuleItemListParser >=> Keyword.Endmodule <&> fun (((((_,a),b),_),c),_) -> 
        match c with
        | None -> { name = a; ports = b; items = [] }
        | Some c -> { name = a; ports = b; items = c })

and ListOfPortsParser inp =
    inp |> (Symbol.LeftRoundBra ?=> PortListParser >=> Symbol.RightRoundBra <&> fun ((_,a),_) ->
        match a with
        | None -> []
        | Some (a) -> a)

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

and PortDeclarationParser inp =
    inp |> buildParser [ 
        Keyword.Input >=> Identifier <&> fun (_,a) -> ItemPort (Input, a) 
        Keyword.Output >=> Identifier <&> fun (_,a) -> ItemPort (Output, a) 
    ]

and AssignParser inp =
    inp |> (Keyword.Assign >=> Identifier >=> Symbol.AssignEqual >=> ExpressionParser <&> fun (((_,a),_),b) -> 
        ItemAssign (a,b))

and WireDeclarationParser inp =
    inp |> (Keyword.Wire ?=> (Symbol.LeftSquareBra >=> Number.Value >=> Symbol.Colon >=> Number.Value >=> Symbol.RightSquareBra) >=> Identifier <&> fun ((_,range),iden) ->
        match range with
        | None -> ItemWireDecl (Single, iden)
        | Some ((((_,a),_),b),_) -> ItemWireDecl (Range (a,b), iden))

and ItemInstantiationParser inp =
    inp |> (Identifier >=> Identifier >=> Symbol.LeftRoundBra ?=> ExpressionListParser >=> Symbol.RightRoundBra <&> fun ((((a,b),_),c),_) ->
        match c with
        | None -> ItemInstantiation (a, b, [])
        | Some lst -> ItemInstantiation (a, b, lst))