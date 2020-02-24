module Verishot.Parser

open System.Text.RegularExpressions
open Verishot.CoreTypes.VerilogAST
open Verishot.CoreTypes

/// result of parser needs to contain return element, lineNum, char/token Num, remLst (Can use result -- error doesnt need return element but will have message)
type ParserResult<'a> = Result<'a * char list, string * char list>

/// take as input line num, char num, char list
type Parser<'a> = char list -> ParserResult<'a>

/// Either p1 or p2 (p1 takes precidence)
/// If there was an error, take the one that got furthest (p1 takes precidence if the same)
let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    fun inp ->
        match p1 inp with
        | Ok res -> Ok res
        | Error (_, rest1) as err1 ->
            match p2 inp with
            | Ok res -> Ok res
            | Error (_, rest2) as err2 ->
                if List.length rest1 > List.length rest2
                then err2
                else err1

/// First apply p1 and then p2
/// Outputs returned in a tuple to be unpacked by user
let (>=>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b> =
    fun inp ->
        match p1 inp with
        | Ok (a, rest) ->
            match p2 rest with
            | Ok (b, rest') -> Ok ((a, b), rest')
            | Error err -> Error err
        | Error err -> Error err

/// Reverse map function for Parser type
let (<&>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    p
    >> function
    | Error err -> Error err
    | Ok (out, rest) -> Ok (f out, rest)

/// Replacement function for Parser type
let (>|>) (p: Parser<'a>) (newItem: 'b) : Parser<'b> =
    p
    >> function
    | Error err -> Error err
    | Ok (_, rest) -> Ok (newItem, rest)

/// Optional combinator (do first, then try second)
/// This operator removed the needd for memoisation
/// Memoisation was very resource intensive bc of the large number of layers
let (?=>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b option> =
    fun inp ->
        match p1 inp with
        | Error err -> Error err
        | Ok (a, rest) ->
            match p2 rest with
            | Error _ -> Ok ((a, None), rest)
            | Ok (b, rest') -> Ok ((a, Some b), rest')

/// Make parser from list
let buildParser lst =
    List.reduce (<|>) lst

/// All parsing instructions for tokens
module Token =

    module Tools =
        /// Remove leading whitespace
        let removeWhitespace =
            let whiteSpaceLst = [' ';'\t';'\n';'\v';'\f';'\r']
            List.skipWhile (fun elem -> List.contains elem whiteSpaceLst)

        /// Match exact string passed in
        let stringParse pattern : Parser<string> =
            let pat = pattern |> List.ofSeq
            fun cLst ->
                let rec takeWhile pat cLst =
                    match pat, cLst with
                    | hd1::tl1, hd2::tl2 when hd1 = hd2 -> takeWhile tl1 tl2
                    | [], remaining -> Ok (pattern, remaining)
                    | _ -> Error (sprintf "Could not match. Expected \'%s\'." pattern, cLst)
                cLst |> removeWhitespace |> takeWhile pat

        /// Match regex expression passed in
        let regParse pattern : Parser<string> =
            let reg = Regex("^" + pattern)  // make once use many times
            fun cLst ->
                let cLst = cLst |> removeWhitespace
                let regMatch =
                    cLst
                    |> List.toArray
                    |> System.String
                    |> reg.Match
                if regMatch.Success
                then Ok (regMatch.Value, List.skip regMatch.Length cLst)
                else Error (sprintf "Could not match. Expected regex of pattern \'%s\'." pattern, cLst)

        let charToInt c = int c - int '0'

        let binToDec str =
            let inp = str |> List.ofSeq |> List.rev |> List.indexed
            let folder state elem =
                state + (2.0 ** float (fst elem)) * float (snd elem |> charToInt)
            (0.0, inp) ||> List.fold folder |> int

        let octToDec str =
            let inp = str |> List.ofSeq |> List.rev |> List.indexed
            let folder state elem =
                state + (8.0 ** float (fst elem)) * float (snd elem |> charToInt)
            (0.0, inp) ||> List.fold folder |> int
           
        let hexToDec str =
            let hexDigtoDex =
                function
                | c when c >= '0' && c <= '9' -> int c - int '0'
                | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
                | c -> int c - int 'a' + 10
            let inp = str |> List.ofSeq |> List.rev |> List.indexed
            let folder state elem =
                state + (16.0 ** float (fst elem)) * float (snd elem |> hexDigtoDex)
            (0.0, inp) ||> List.fold folder |> int
            

    let Identifier = Tools.regParse "[a-zA-Z_]+[a-zA-Z'_0-9]*"
    let ExpressionIdentifier = Identifier <&> ExprIdentifier

    module Keyword =
        let And = Tools.stringParse "and"
        let Assign = Tools.stringParse "assign"
        let Begin = Tools.stringParse "begin"
        let Case = Tools.stringParse "case"
        let Deassign = Tools.stringParse "deassign"
        let Default = Tools.stringParse "default"
        let Defparam = Tools.stringParse "defparam"
        let Disable = Tools.stringParse "disable"
        let Else = Tools.stringParse "else"
        let End = Tools.stringParse "end"
        let Endcase = Tools.stringParse "endcase"
        let Endmodule = Tools.stringParse "endmodule"
        let For = Tools.stringParse "for"
        let Forever = Tools.stringParse "forever"
        let If = Tools.stringParse "if"
        let Ifnone = Tools.stringParse "ifnone"
        let Include = Tools.stringParse "include"
        let Instance = Tools.stringParse "instance"
        let Initial = Tools.stringParse "initial"
        let Inout = Tools.stringParse "inout"
        let Input = Tools.stringParse "input"
        let Integer = Tools.stringParse "integer"
        let Library = Tools.stringParse "library"

    module Symbol =
        let LeftRoundBra = Tools.stringParse "("
        let RightRoundBra = Tools.stringParse ")"
        let LeftSquareBra = Tools.stringParse "["
        let RightSquareBra = Tools.stringParse "]"
        let LeftCurlyBra = Tools.stringParse "{"
        let RightCurlyBra = Tools.stringParse "}"
        let UnaryPlus = Tools.stringParse "+" >|> UOpPlus
        let UnaryMinus = Tools.stringParse "-" >|> UOpMinus
        let BinPlus = Tools.stringParse "+" >|> BOpPlus
        let BinMinus = Tools.stringParse "-" >|> BOpMinus
        let Star = Tools.stringParse "*" >|> BOpStar
        let Divide = Tools.stringParse "/" >|> BOpDiv
        let Exponent = Tools.stringParse "**" >|> BOpExponent
        let Modulus = Tools.stringParse "%" >|> BOpMod
        let GreaterThan = Tools.stringParse ">" >|> BOpGreaterThan
        let LessThan = Tools.stringParse "<" >|> BOpLessThan
        let GreaterThanOrEqual = Tools.stringParse ">=" >|> BOpGreaterThanEqual
        let LessThanOrEqual = Tools.stringParse "<=" >|> BOpLessThanEqual
        let Bang = Tools.stringParse "!" >|> UOpBang
        let LogicalAnd = Tools.stringParse "&&" >|> BOpLogicalAnd
        let LogicalOr = Tools.stringParse "||" >|> BOpLogicalOr
        let LogicalEqual = Tools.stringParse "==" >|> BOpEquals
        let LogicalNotEqual = Tools.stringParse "!=" >|> BOpBangEquals
        let CaseEqual = Tools.stringParse "===" >|> BOpTripleEquals
        let CaseNotEqual = Tools.stringParse "!==" >|> BOpBangTripleEquals
        let BitwiseNegation = Tools.stringParse "~" >|> UOpBitwiseNegation
        let BitwiseAnd = Tools.stringParse "&" >|> BOpBitwiseAnd
        let BitwiseNand = Tools.stringParse "~&" >|> BOpBitwiseNAnd
        let BitwiseOr = Tools.stringParse "|" >|> BOpBitwiseOr
        let BitwiseNor = Tools.stringParse "~|" >|> BOpBitwiseNOr
        let BitwiseExclusiveOr = Tools.stringParse "^" >|> BOpXor
        let BitwiseExclusiveNor = (Tools.stringParse "^~" <|> Tools.stringParse "~^") >|> BOpXNor
        let ReductionAnd = Tools.stringParse "&" >|> UOpAndReduce
        let ReductionNand = Tools.stringParse "~&" >|> UOpNAndReduce
        let ReductionOr = Tools.stringParse "|" >|> UOpOrReduce
        let ReductionNor = Tools.stringParse "~|" >|> UOpNOrReduce
        let ReductionXor = Tools.stringParse "^" >|> UOpXOrReduce
        let ReductionXnor = (Tools.stringParse "~^" <|> Tools.stringParse "^~") >|> UOpXNorReduce
        let LogicalLeftShift = Tools.stringParse "<<" >|> BOpLogicLeftShift
        let LogicalRightShift = Tools.stringParse ">>" >|> BOpLogicRightShift
        let ArithmaticLeftShift = Tools.stringParse "<<<" >|> BOpArithmeticLeftShift
        let ArithmaticRightShift = Tools.stringParse ">>>" >|> BOpArithmeticRightShift
        let QuestionMark = Tools.stringParse "?"
        let Colon = Tools.stringParse ":"
        let Comma = Tools.stringParse ","

    // let Number = 
    //     Tools.regParse "[0-9]+"
    //     >> function
    //     | Error err -> Error err
    //     | Ok (numStr, rest) -> Ok (int numStr, rest)
    module Number =
        let numParse =
            let unsignedNumParse = Tools.regParse "[0-9]+"
            let decimalNumParse = 
                let decimalBaseStr = buildParser [ Tools.stringParse "'d" ; Tools.stringParse "'D" ]
                unsignedNumParse >=> decimalBaseStr >=> unsignedNumParse <&> fun ((a,_),b) -> Some (int a), int b
            let binaryNumParse =
                let unsignedBinNum = Tools.regParse "[01]+"
                let binaryBaseStr = buildParser [ Tools.stringParse "'b" ; Tools.stringParse "'B" ]
                unsignedNumParse >=> binaryBaseStr >=> unsignedBinNum <&> fun ((a,_),b) -> Some (int a), Tools.binToDec b
            let octalNumParse =
                let unsignedOctNum = Tools.regParse "[0-7]+"
                let octalBaseStr = buildParser [ Tools.stringParse "'o" ; Tools.stringParse "'O" ]
                unsignedNumParse >=> octalBaseStr >=> unsignedOctNum <&> fun ((a,_),b) -> Some (int a), Tools.octToDec b
            let hexNumParse =
                let unsignedHexNum = Tools.regParse "[0-9a-fA-F]+"
                let hexBaseStr = buildParser [ Tools.stringParse "'h" ; Tools.stringParse "'H" ]
                unsignedNumParse >=> hexBaseStr >=> unsignedHexNum <&> fun ((a,_),b) -> Some (int a), Tools.hexToDec b
            buildParser [
                decimalNumParse
                binaryNumParse
                octalNumParse
                hexNumParse
                unsignedNumParse <&> fun a -> None, int a
            ]
        
        let Expr = numParse <&> ExprNumber

        let Value = numParse <&> fun (_, a) -> a


/// All parsing instructions for expressions
module Expression =

    module Tools =
        /// To map output of combinator to expected binary expression
        let OpBinExprMap (a, b) =
            match b with
            | None -> a
            | Some (b, c) -> ExprBinary (a,b,c)

        let MemoiseParser fn =
            let mutable cache = Map []
            fun inp -> 
                if Map.containsKey inp cache
                then cache.[inp] 
                else 
                    let res = fn inp
                    cache <- Map.add inp res cache
                    res

    /// Removing the inp from this definition creates an annoying warning
    let rec TermParser inp =
        inp
        |> buildParser [
            Token.Number.Expr
            Token.ExpressionIdentifier
            Token.Symbol.LeftRoundBra >=> ExpressionParser >=> Token.Symbol.RightRoundBra <&> (fun ((_,b),_) -> b)
        ]

    and UnaryOpParser inp =
        let operator = buildParser [
            Token.Symbol.UnaryPlus
            Token.Symbol.UnaryMinus
            Token.Symbol.Bang
            Token.Symbol.BitwiseNegation
            Token.Symbol.ReductionAnd
            Token.Symbol.ReductionOr
            Token.Symbol.ReductionXor
            Token.Symbol.ReductionNand
            Token.Symbol.ReductionNor
            Token.Symbol.ReductionXnor
        ]
        inp
        |>buildParser [
            operator >=> UnaryOpParser <&> ExprUnary
            TermParser
        ]

    and AddSubParser inp =
        let operator = buildParser [
            Token.Symbol.BinPlus
            Token.Symbol.BinMinus
        ]
        inp |> (UnaryOpParser ?=> (operator >=> AddSubParser) <&> Tools.OpBinExprMap)

    and MulDivModParser inp =
        let operator = buildParser [
            Token.Symbol.Star
            Token.Symbol.Divide
            Token.Symbol.Modulus
        ]
        inp |> (AddSubParser ?=> (operator >=> MulDivModParser) <&> Tools.OpBinExprMap)

    and ExponentParser inp =
        inp |> (MulDivModParser ?=> (Token.Symbol.Exponent >=> ExponentParser) <&> Tools.OpBinExprMap)

    and ShiftParser inp =
        let operator = buildParser [
            Token.Symbol.LogicalLeftShift
            Token.Symbol.LogicalRightShift
            Token.Symbol.ArithmaticLeftShift
            Token.Symbol.ArithmaticRightShift
        ]
        inp |> (ExponentParser ?=> (operator >=> ShiftParser) <&> Tools.OpBinExprMap)

    and RelationalParser inp =
        let operator = buildParser [
            Token.Symbol.GreaterThan
            Token.Symbol.LessThan
            Token.Symbol.GreaterThanOrEqual
            Token.Symbol.LessThanOrEqual
        ]
        inp |> (ShiftParser ?=> (operator >=> RelationalParser) <&> Tools.OpBinExprMap)
    
    and RelationalEqualityParser inp =
        let operator = buildParser [
            Token.Symbol.LogicalEqual
            Token.Symbol.LogicalNotEqual
            Token.Symbol.CaseEqual
            Token.Symbol.CaseNotEqual
        ]
        inp |> (RelationalParser ?=> (operator >=> RelationalEqualityParser) <&> Tools.OpBinExprMap)

    and BitwiseAndParser inp =
        let operator = buildParser [
            Token.Symbol.BitwiseAnd
            Token.Symbol.BitwiseNand
        ]
        inp |> (RelationalEqualityParser ?=> (operator >=> BitwiseAndParser) <&> Tools.OpBinExprMap)

    and BitwiseXorParser inp =
        let operator = buildParser [
            Token.Symbol.BitwiseExclusiveOr
            Token.Symbol.BitwiseExclusiveNor
        ]
        inp |> (BitwiseAndParser ?=> (operator >=> BitwiseXorParser) <&> Tools.OpBinExprMap)

    and BitwiseOrParser inp =
        let operator = buildParser [
            Token.Symbol.BitwiseOr
            Token.Symbol.BitwiseNor
        ]
        inp |> (BitwiseXorParser ?=> (operator >=> BitwiseOrParser) <&> Tools.OpBinExprMap)

    and LogicalAndParser inp =
        inp |> (BitwiseOrParser ?=> (Token.Symbol.LogicalAnd >=> LogicalAndParser) <&> Tools.OpBinExprMap)

    and LogicalOrParser inp =
        inp |> (LogicalAndParser ?=> (Token.Symbol.LogicalOr >=> LogicalOrParser) <&> Tools.OpBinExprMap)

    and ConditionalParser inp =
        inp |> (LogicalOrParser ?=> (Token.Symbol.QuestionMark >=> ConditionalParser >=> Token.Symbol.Colon >=> ConditionalParser) <&> (fun (a,b) ->
            match b with
            | None -> a
            | Some (((_,b),_),c) -> ExprIfThenElse (a,b,c)))

    and ExpressionListParser inp =
        inp
        |> buildParser [
            ConditionalParser >=> Token.Symbol.Comma >=> ExpressionListParser <&> 
                function
                | ((a, _), ExprConcateneation b) -> ExprConcateneation (a::b)
                | _ -> failwithf "can't reach"
            ConditionalParser <&> (fun a -> ExprConcateneation [a])
        ]

    and ExpressionConcatParser inp =
        inp
        |> buildParser [
            Token.Symbol.LeftCurlyBra >=> ExpressionListParser >=> Token.Symbol.RightCurlyBra <&> (fun ((_,a),_) -> a)
            ConditionalParser
        ]

    and IndexParser inp =
        inp |> (Token.Number.Value ?=> (Token.Symbol.Colon >=> Token.Number.Value) <&> fun (a,b) ->
            match b with
            | None -> IndexNum a
            | Some (_, b) -> IndexRange (a,b))

    and ExpressionParser inp =
        inp |> (ExpressionConcatParser ?=> (Token.Symbol.LeftSquareBra >=> IndexParser >=> Token.Symbol.RightSquareBra) <&> fun (a,b) ->
            match b with
            | None -> a
            | Some ((_,b),_) -> ExprIndex (a,b))


