module Verishot.Token

open System.Text.RegularExpressions
open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Util

module TokTools =
    /// Remove leading whitespace
    let removeWhitespace =
        let whiteSpaceLst = [' ';'\t';'\n';'\v';'\f';'\r']
        List.skipWhile (fun elem -> List.contains elem whiteSpaceLst)

    /// Remove all characters that are part of a comment
    /// True if comments were removed
    let removeComments = 
        function
        | hd1 :: hd2 :: tl when hd1 = '/' && hd2 = '/' -> true, tl |> List.skipWhile ((<>) '\n') |> List.safeSkip 1
        | hd1 :: hd2 :: tl when hd1 = '/' && hd2 = '*' -> true, tl |> List.skipWhile2 (fun h1 h2 -> (h1 <> '*') && (h2 <> '/')) |> List.safeSkip 2
        | lst -> false, lst

    /// Remove all whitespace and comments
    let rec whiteSpaceAndComments lst =
        let lst' = lst |> removeWhitespace
        lst'
        |> removeComments
        |> function
        | true, lst'' -> whiteSpaceAndComments lst''
        | false, lst'' -> lst''

    /// Match exact string passed in
    let stringParse pattern : Parser<string> =
        let pat = pattern |> List.ofSeq
        fun cLst ->
            let rec takeWhile pat cLst =
                match pat, cLst with
                | hd1::tl1, hd2::tl2 when hd1 = hd2 -> takeWhile tl1 tl2
                | [], remaining -> Ok (pattern, remaining, None)
                | _ -> Error (sprintf "Could not match. Expected \'%s\'." pattern, cLst)
            cLst |> whiteSpaceAndComments |> takeWhile pat

    /// Match regex expression passed in
    let regParse pattern : Parser<string> =
        let reg = Regex("^" + pattern)  // make once use many times
        fun cLst ->
            let cLst = cLst |> whiteSpaceAndComments
            let regMatch =
                cLst
                |> List.toArray
                |> System.String
                |> reg.Match
            if regMatch.Success
            then Ok (regMatch.Value, List.skip regMatch.Length cLst, None)
            else Error (sprintf "Could not match. Expected regex of pattern \'%s\'." pattern, cLst)

    let strictListIn (followChars: char list) (p: string -> Parser<'a>) (pattern: string) =
        fun cLst ->
            match p pattern cLst with
            | Error err -> Error err
            | Ok (res, hd::tl, opErr) when not <| List.contains hd followChars -> Ok (res, hd::tl, opErr)
            | Ok (res, [], opErr) -> Ok (res, [], opErr)
            | _ -> Error (sprintf "Could not match. Expected pattern: \'%s\'." pattern, cLst)

    /// Strict parse will not allow the following character to be something that could be part of an identifier
    let strict =
        ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9'] @ ['\'';'_'] |> strictListIn

    /// Bitwise and cannot be followed by an ampersand
    let strictAnd (p: string -> Parser<'a>) (pattern: string) =
        (p, pattern) ||> strictListIn ['&']

    /// Bitwise or cannot be followed by an pipe
    let strictOr (p: string -> Parser<'a>) (pattern: string) =
        (p, pattern) ||> strictListIn ['|']

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

let Identifier = TokTools.strict TokTools.regParse "[a-zA-Z_]+[a-zA-Z'_0-9]*"
let ExpressionIdentifier = Identifier <&> ExprIdentifier

module Keyword =
    let Assign = TokTools.strict TokTools.stringParse "assign"
    let Endmodule = TokTools.strict TokTools.stringParse "endmodule"
    let Input = TokTools.strict TokTools.stringParse "input"
    let Module = TokTools.strict TokTools.stringParse "module"
    let Output = TokTools.strict TokTools.stringParse "output"
    let Wire = TokTools.strict TokTools.stringParse "wire"

module Symbol =
    let LeftRoundBra = TokTools.stringParse "("
    let RightRoundBra = TokTools.stringParse ")"
    let LeftSquareBra = TokTools.stringParse "["
    let RightSquareBra = TokTools.stringParse "]"
    let LeftCurlyBra = TokTools.stringParse "{"
    let RightCurlyBra = TokTools.stringParse "}"
    let UnaryPlus = TokTools.stringParse "+" >|> UOpPlus
    let UnaryMinus = TokTools.stringParse "-" >|> UOpMinus
    let BinPlus = TokTools.stringParse "+" >|> BOpPlus
    let BinMinus = TokTools.stringParse "-" >|> BOpMinus
    let Star = TokTools.stringParse "*" >|> BOpStar
    let Divide = TokTools.stringParse "/" >|> BOpDiv
    let Exponent = TokTools.stringParse "**" >|> BOpExponent
    let Modulus = TokTools.stringParse "%" >|> BOpMod
    let GreaterThan = TokTools.stringParse ">" >|> BOpGreaterThan
    let LessThan = TokTools.stringParse "<" >|> BOpLessThan
    let GreaterThanOrEqual = TokTools.stringParse ">=" >|> BOpGreaterThanEqual
    let LessThanOrEqual = TokTools.stringParse "<=" >|> BOpLessThanEqual
    let Bang = TokTools.stringParse "!" >|> UOpBang
    let LogicalAnd = TokTools.stringParse "&&" >|> BOpLogicalAnd
    let LogicalOr = TokTools.stringParse "||" >|> BOpLogicalOr
    let LogicalEqual = TokTools.stringParse "==" >|> BOpEquals
    let LogicalNotEqual = TokTools.stringParse "!=" >|> BOpBangEquals
    let CaseEqual = TokTools.stringParse "===" >|> BOpTripleEquals
    let CaseNotEqual = TokTools.stringParse "!==" >|> BOpBangTripleEquals
    let BitwiseNegation = TokTools.stringParse "~" >|> UOpBitwiseNegation
    let BitwiseAnd = TokTools.strictAnd TokTools.stringParse "&" >|> BOpBitwiseAnd
    let BitwiseNand = TokTools.stringParse "~&" >|> BOpBitwiseNAnd
    let BitwiseOr = TokTools.strictOr TokTools.stringParse "|" >|> BOpBitwiseOr
    let BitwiseNor = TokTools.stringParse "~|" >|> BOpBitwiseNOr
    let BitwiseExclusiveOr = TokTools.stringParse "^" >|> BOpXor
    let BitwiseExclusiveNor = (TokTools.stringParse "^~" <|> TokTools.stringParse "~^") >|> BOpXNor
    let ReductionAnd = TokTools.stringParse "&" >|> UOpAndReduce
    let ReductionNand = TokTools.stringParse "~&" >|> UOpNAndReduce
    let ReductionOr = TokTools.stringParse "|" >|> UOpOrReduce
    let ReductionNor = TokTools.stringParse "~|" >|> UOpNOrReduce
    let ReductionXor = TokTools.stringParse "^" >|> UOpXOrReduce
    let ReductionXnor = (TokTools.stringParse "~^" <|> TokTools.stringParse "^~") >|> UOpXNorReduce
    let LogicalLeftShift = TokTools.stringParse "<<" >|> BOpLogicLeftShift
    let LogicalRightShift = TokTools.stringParse ">>" >|> BOpLogicRightShift
    let ArithmaticLeftShift = TokTools.stringParse "<<<" >|> BOpArithmeticLeftShift
    let ArithmaticRightShift = TokTools.stringParse ">>>" >|> BOpArithmeticRightShift
    let QuestionMark = TokTools.stringParse "?"
    let Colon = TokTools.stringParse ":"
    let Comma = TokTools.stringParse ","
    let Semicolon = TokTools.stringParse ";"
    let AssignEqual = TokTools.stringParse "="

module Number =
    let numParse =
        let unsignedNumParse = TokTools.regParse "[0-9]+"
        let decimalNumParse = 
            let decimalBaseStr = buildParser [ TokTools.stringParse "'d" ; TokTools.stringParse "'D" ]
            unsignedNumParse >=> decimalBaseStr >=> unsignedNumParse <&> fun ((a,_),b) -> Some (int a), int b
        let binaryNumParse =
            let unsignedBinNum = TokTools.regParse "[01]+"
            let binaryBaseStr = buildParser [ TokTools.stringParse "'b" ; TokTools.stringParse "'B" ]
            unsignedNumParse >=> binaryBaseStr >=> unsignedBinNum <&> fun ((a,_),b) -> Some (int a), TokTools.binToDec b
        let octalNumParse =
            let unsignedOctNum = TokTools.regParse "[0-7]+"
            let octalBaseStr = buildParser [ TokTools.stringParse "'o" ; TokTools.stringParse "'O" ]
            unsignedNumParse >=> octalBaseStr >=> unsignedOctNum <&> fun ((a,_),b) -> Some (int a), TokTools.octToDec b
        let hexNumParse =
            let unsignedHexNum = TokTools.regParse "[0-9a-fA-F]+"
            let hexBaseStr = buildParser [ TokTools.stringParse "'h" ; TokTools.stringParse "'H" ]
            unsignedNumParse >=> hexBaseStr >=> unsignedHexNum <&> fun ((a,_),b) -> Some (int a), TokTools.hexToDec b
        buildParser [
            decimalNumParse
            binaryNumParse
            octalNumParse
            hexNumParse
            unsignedNumParse <&> fun a -> None, int a
        ]
    
    let Expr = numParse <&> ExprNumber

    let Value = numParse <&> fun (_, a) -> a