module Verishot.Expression

open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Token

/// ExprTools module for any utility function designed for expressions
module ExprTools =
    /// To map output of combinator to expected binary expression
    /// If the second failed, then just return the first without wrapping it
    let OpBinExprMap (a, b) =
        match b with
        | None -> a
        | Some (b, c) -> ExprBinary (a,b,c)

/// The order of the parsers in this file dictate the order of precidence in verilog expressions

/// Removing the inp from this definition creates an annoying warning
/// Parses any terminal expressions (numbers/identifiers)
/// Or can be any expression enclosed in round brackets
let rec TermParser inp =
    inp |> buildParser [
        Number.Expr
        ExpressionIdentifier
        Symbol.LeftRoundBra >=> ExpressionParser >=> Symbol.RightRoundBra <&> (fun ((_,b),_) -> b)
    ]

/// Use 'and' because all rules are mutually recursive for expressions
/// Parses any unary expression
/// + - ! ~ & | ^ ~& ~| ~^
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
    inp |> buildParser [
        operator >=> UnaryOpParser <&> ExprUnary
        TermParser
    ]

/// Parses addition or subtraction
/// + -
and AddSubParser inp =
    let operator = buildParser [
        Symbol.BinPlus
        Symbol.BinMinus
    ]
    inp |> (UnaryOpParser ?=> (operator >=> AddSubParser) <&> ExprTools.OpBinExprMap)

/// Parses multiplication, division, and mod
/// * / %
and MulDivModParser inp =
    let operator = buildParser [
        Symbol.Star
        Symbol.Divide
        Symbol.Modulus
    ]
    inp |> (AddSubParser ?=> (operator >=> MulDivModParser) <&> ExprTools.OpBinExprMap)

/// Parses exponent
/// **
and ExponentParser inp =
    inp |> (MulDivModParser ?=> (Symbol.Exponent >=> ExponentParser) <&> ExprTools.OpBinExprMap)

/// Parses shifts
/// << >> <<< >>>
and ShiftParser inp =
    let operator = buildParser [
        Symbol.ArithmaticLeftShift
        Symbol.ArithmaticRightShift
        Symbol.LogicalLeftShift
        Symbol.LogicalRightShift
    ]
    inp |> (ExponentParser ?=> (operator >=> ShiftParser) <&> ExprTools.OpBinExprMap)

/// Parses relational logic
/// < > <= >=
and RelationalParser inp =
    let operator = buildParser [
        Symbol.GreaterThanOrEqual
        Symbol.LessThanOrEqual
        Symbol.GreaterThan
        Symbol.LessThan
    ]
    inp |> (ShiftParser ?=> (operator >=> RelationalParser) <&> ExprTools.OpBinExprMap)

/// Parses relational equality
/// == != === !==
and RelationalEqualityParser inp =
    let operator = buildParser [
        Symbol.CaseEqual
        Symbol.CaseNotEqual
        Symbol.LogicalEqual
        Symbol.LogicalNotEqual
    ]
    inp |> (RelationalParser ?=> (operator >=> RelationalEqualityParser) <&> ExprTools.OpBinExprMap)

/// Parses bitwise and and nand
/// & ~&
and BitwiseAndParser inp =
    let operator = buildParser [
        Symbol.BitwiseAnd
        Symbol.BitwiseNand
    ]
    inp |> (RelationalEqualityParser ?=> (operator >=> BitwiseAndParser) <&> ExprTools.OpBinExprMap)

/// Parses bitwise xor
/// ^ ~^
and BitwiseXorParser inp =
    let operator = buildParser [
        Symbol.BitwiseExclusiveOr
        Symbol.BitwiseExclusiveNor
    ]
    inp |> (BitwiseAndParser ?=> (operator >=> BitwiseXorParser) <&> ExprTools.OpBinExprMap)

/// Parses bitwise or
/// | ~|
and BitwiseOrParser inp =
    let operator = buildParser [
        Symbol.BitwiseOr
        Symbol.BitwiseNor
    ]
    inp |> (BitwiseXorParser ?=> (operator >=> BitwiseOrParser) <&> ExprTools.OpBinExprMap)

/// Parses logical and
/// &&
and LogicalAndParser inp =
    inp |> (BitwiseOrParser ?=> (Symbol.LogicalAnd >=> LogicalAndParser) <&> ExprTools.OpBinExprMap)

/// Parses logical or
/// ||
and LogicalOrParser inp =
    inp |> (LogicalAndParser ?=> (Symbol.LogicalOr >=> LogicalOrParser) <&> ExprTools.OpBinExprMap)

/// Parses conditional expressions
/// a ? b : c
and ConditionalParser inp =
    inp |> (LogicalOrParser ?=> (Symbol.QuestionMark >=> ConditionalParser >=> Symbol.Colon >=> ConditionalParser) <&> (fun (a,b) ->
        match b with
        | None -> a
        | Some (((_,b),_),c) -> ExprIfThenElse (a,b,c)))

/// Parses list of expression (only for another level)
and SubExpressionListParser inp =
    inp |> buildParser [
        ConditionalParser >=> Symbol.Comma >=> SubExpressionListParser <&> 
            function
            | ((a, _), ExprConcateneation b) -> ExprConcateneation (a::b)
            | _ -> failwithf "can't reach"
        ConditionalParser <&> (fun a -> ExprConcateneation [a])
    ]

/// Parses concat expression (list wrapped in {})
and ExpressionConcatParser inp =
    inp |> buildParser [
        Symbol.LeftCurlyBra >=> SubExpressionListParser >=> Symbol.RightCurlyBra <&> (fun ((_,a),_) -> a)
        ConditionalParser
    ]

/// Parses the index given inside of a set of square brackets (without the brackets)
and IndexParser inp =
    inp |> (Number.Value ?=> (Symbol.Colon >=> Number.Value) <&> fun (a,b) ->
        match b with
        | None -> IndexNum a
        | Some (_, b) -> IndexRange (a,b))

/// Parses other expressions but indexed with square brackets (top level)
and ExpressionParser inp =
    inp |> (ExpressionConcatParser ?=> (Symbol.LeftSquareBra >=> IndexParser >=> Symbol.RightSquareBra) <&> fun (a,b) ->
        match b with
        | None -> a
        | Some ((_,b),_) -> ExprIndex (a,b))

/// Parses a list of expressions seperated by commas
/// Used by non expression parsers (like argument list)
and ExpressionListParser inp =
    inp |> (ExpressionParser ?=> (Symbol.Comma >=> ExpressionListParser) <&> fun (a,b) ->
        match b with
        | None -> [a]
        | Some (_, lst) -> a::lst)