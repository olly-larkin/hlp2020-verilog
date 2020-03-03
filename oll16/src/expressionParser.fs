module Verishot.Expression

open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Token

module ExprTools =
    /// To map output of combinator to expected binary expression
    let OpBinExprMap (a, b) =
        match b with
        | None -> a
        | Some (b, c) -> ExprBinary (a,b,c)

/// Removing the inp from this definition creates an annoying warning
let rec TermParser inp =
    inp
    |> buildParser [
        Number.Expr
        ExpressionIdentifier
        Symbol.LeftRoundBra >=> ExpressionParser >=> Symbol.RightRoundBra <&> (fun ((_,b),_) -> b)
    ]

and UnaryOpParser inp =
    let operator = buildParser [
        Symbol.UnaryPlus
        Symbol.UnaryMinus
        Symbol.Bang
        Symbol.BitwiseNegation
        Symbol.ReductionAnd
        Symbol.ReductionOr
        Symbol.ReductionXor
        Symbol.ReductionNand
        Symbol.ReductionNor
        Symbol.ReductionXnor
    ]
    inp
    |>buildParser [
        operator >=> UnaryOpParser <&> ExprUnary
        TermParser
    ]

and AddSubParser inp =
    let operator = buildParser [
        Symbol.BinPlus
        Symbol.BinMinus
    ]
    inp |> (UnaryOpParser ?=> (operator >=> AddSubParser) <&> ExprTools.OpBinExprMap)

and MulDivModParser inp =
    let operator = buildParser [
        Symbol.Star
        Symbol.Divide
        Symbol.Modulus
    ]
    inp |> (AddSubParser ?=> (operator >=> MulDivModParser) <&> ExprTools.OpBinExprMap)

and ExponentParser inp =
    inp |> (MulDivModParser ?=> (Symbol.Exponent >=> ExponentParser) <&> ExprTools.OpBinExprMap)

and ShiftParser inp =
    let operator = buildParser [
        Symbol.ArithmaticLeftShift
        Symbol.ArithmaticRightShift
        Symbol.LogicalLeftShift
        Symbol.LogicalRightShift
    ]
    inp |> (ExponentParser ?=> (operator >=> ShiftParser) <&> ExprTools.OpBinExprMap)

and RelationalParser inp =
    let operator = buildParser [
        Symbol.GreaterThanOrEqual
        Symbol.LessThanOrEqual
        Symbol.GreaterThan
        Symbol.LessThan
    ]
    inp |> (ShiftParser ?=> (operator >=> RelationalParser) <&> ExprTools.OpBinExprMap)

and RelationalEqualityParser inp =
    let operator = buildParser [
        Symbol.CaseEqual
        Symbol.CaseNotEqual
        Symbol.LogicalEqual
        Symbol.LogicalNotEqual
    ]
    inp |> (RelationalParser ?=> (operator >=> RelationalEqualityParser) <&> ExprTools.OpBinExprMap)

and BitwiseAndParser inp =
    let operator = buildParser [
        Symbol.BitwiseAnd
        Symbol.BitwiseNand
    ]
    inp |> (RelationalEqualityParser ?=> (operator >=> BitwiseAndParser) <&> ExprTools.OpBinExprMap)

and BitwiseXorParser inp =
    let operator = buildParser [
        Symbol.BitwiseExclusiveOr
        Symbol.BitwiseExclusiveNor
    ]
    inp |> (BitwiseAndParser ?=> (operator >=> BitwiseXorParser) <&> ExprTools.OpBinExprMap)

and BitwiseOrParser inp =
    let operator = buildParser [
        Symbol.BitwiseOr
        Symbol.BitwiseNor
    ]
    inp |> (BitwiseXorParser ?=> (operator >=> BitwiseOrParser) <&> ExprTools.OpBinExprMap)

and LogicalAndParser inp =
    inp |> (BitwiseOrParser ?=> (Symbol.LogicalAnd >=> LogicalAndParser) <&> ExprTools.OpBinExprMap)

and LogicalOrParser inp =
    inp |> (LogicalAndParser ?=> (Symbol.LogicalOr >=> LogicalOrParser) <&> ExprTools.OpBinExprMap)

and ConditionalParser inp =
    inp |> (LogicalOrParser ?=> (Symbol.QuestionMark >=> ConditionalParser >=> Symbol.Colon >=> ConditionalParser) <&> (fun (a,b) ->
        match b with
        | None -> a
        | Some (((_,b),_),c) -> ExprIfThenElse (a,b,c)))

and SubExpressionListParser inp =
    inp
    |> buildParser [
        ConditionalParser >=> Symbol.Comma >=> SubExpressionListParser <&> 
            function
            | ((a, _), ExprConcateneation b) -> ExprConcateneation (a::b)
            | _ -> failwithf "can't reach"
        ConditionalParser <&> (fun a -> ExprConcateneation [a])
    ]

and ExpressionConcatParser inp =
    inp
    |> buildParser [
        Symbol.LeftCurlyBra >=> SubExpressionListParser >=> Symbol.RightCurlyBra <&> (fun ((_,a),_) -> a)
        ConditionalParser
    ]

and IndexParser inp =
    inp |> (Number.Value ?=> (Symbol.Colon >=> Number.Value) <&> fun (a,b) ->
        match b with
        | None -> IndexNum a
        | Some (_, b) -> IndexRange (a,b))

and ExpressionParser inp =
    inp |> (ExpressionConcatParser ?=> (Symbol.LeftSquareBra >=> IndexParser >=> Symbol.RightSquareBra) <&> fun (a,b) ->
        match b with
        | None -> a
        | Some ((_,b),_) -> ExprIndex (a,b))

and ExpressionListParser inp =
    inp |> (ExpressionParser ?=> (Symbol.Comma >=> ExpressionListParser) <&> fun (a,b) ->
        match b with
        | None -> [a]
        | Some (_, lst) -> a::lst)