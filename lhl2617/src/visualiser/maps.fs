module Verishot.VisualiseMaps
open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST

let BinaryOpMap: Map<VerilogAST.BinaryOp, string> =
    Map [
        BOpPlus, "+"
        BOpMinus, "-"
        BOpStar, "*"
        BOpDiv, "/"
        BOpMod, "%"
        BOpEquals, "=="
        BOpBangEquals, "!="
        BOpTripleEquals, "==="
        BOpBangTripleEquals, "!=="
        BOpLogicalAnd, "&&"
        BOpLogicalOr, "||"
        BOpExponent, "**"
        BOpLessThan, "<"
        BOpLessThanEqual, "<="
        BOpGreaterThan, ">"
        BOpGreaterThanEqual, ">="
        BOpBitwiseAnd, "&"
        BOpBitwiseOr, "|"
        BOpBitwiseNAnd, "~&"
        BOpBitwiseNOr, "~|"
        BOpXor, "^"
        BOpXNor, "^~"
        BOpLogicRightShift, "<<"
        BOpLogicLeftShift, ">>"
        BOpArithmeticRightShift, ">>>"
        BOpArithmeticLeftShift, "<<<"
    ]

let UnaryOpMap: Map<VerilogAST.UnaryOp, string> =
    Map [
        UOpPlus, "+"
        UOpMinus, "-"
        UOpBang, "!"
        UOpBitwiseNegation, "~"
        UOpAndReduce, "&"
        UOpNAndReduce, "~&"
        UOpOrReduce, "|"
        UOpNOrReduce, "~|"
        UOpXOrReduce, "^"
        UOpXNorReduce, "^~"
    ]