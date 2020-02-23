module Verishot.VisualiseMaps
open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST

let BinaryOpXMLMap: Map<VerilogAST.BinaryOp, string> =
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
        BOpLogicalAnd, "&amp;&amp;"
        BOpLogicalOr, "||"
        BOpExponent, "**"
        BOpLessThan, "&lt;"
        BOpLessThanEqual, "&lt;="
        BOpGreaterThan, "&gt;"
        BOpGreaterThanEqual, "&gt;="
        BOpBitwiseAnd, "&amp;"
        BOpBitwiseOr, "|"
        BOpBitwiseNAnd, "~&amp;"
        BOpBitwiseNOr, "~|"
        BOpXor, "^"
        BOpXNor, "^~"
        BOpLogicRightShift, "&lt;&lt;"
        BOpLogicLeftShift, "&gt;&gt;"
        BOpArithmeticRightShift, "&lt;&lt;&lt;"
        BOpArithmeticLeftShift, "&gt;&gt;&gt;"
    ]

let UnaryOpXMLMap: Map<VerilogAST.UnaryOp, string> =
    Map [
        UOpPlus, "+"
        UOpMinus, "-"
        UOpBang, "!"
        UOpBitwiseNegation, "~"
        UOpAndReduce, "&amp;"
        UOpNAndReduce, "~&amp;"
        UOpOrReduce, "|"
        UOpNOrReduce, "~|"
        UOpXOrReduce, "^"
        UOpXNorReduce, "^~"
    ]