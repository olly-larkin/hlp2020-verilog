namespace rec Verishot.CoreTypes

type Identifier = string

/// Identifies a module (*not* a module instance)
type ModuleIdentifier =
    | StringIdentifier of string
    | BOpIdentifier of VerilogAST.BinaryOp
    | UOpIdentifier of VerilogAST.UnaryOp

type Range =
    | Single
    | Range of (int * int)

type Direction =
    | Input
    | Output

type ModuleDecl =
    { name: Identifier
      ports: (Direction * Identifier * Range) list }

module Netlist =
    (***
     The way connections are represented in this module is the following:
     Every node (i.e. pin or module instance) holds references for its
     *outgoing* connections.
    *)

    type ConnectionTarget =
        | PinTarget of {| pinName: Identifier; pinIndex: int |}
        | InstanceTarget of {| targetNode: Identifier; portName: Identifier; portIndex: int |}

    type Connection =
        { srcPortIndex: int
          target: ConnectionTarget }

    /// Instance of a module.
    type ModuleInstance =
        { /// Name of the module being declared (first identifier in verilog declaration)
          moduleName: ModuleIdentifier

          /// Name of the instance (second identifier in verilog declaration)
          instanceName: Identifier

          /// *Outgoing* connections of the module
          connections: Map<Identifier, Connection list> }

    type Node =
        /// An input pin of the *module the netlist refers to*
        | InputPin of Identifier * Connection list
        /// An output pin of the *module the netlist refers to*
        | OutputPin of Identifier
        | ModuleInstance of ModuleInstance
        | Constant of {| value: int; width: int |}

    type Netlist =
        { nodes: Node list
          moduleName: Identifier }


module VerilogAST =
    type UnaryOp =
        /// +
        | UOpPlus
        /// -
        | UOpMinus
        /// !
        | UOpBang
        /// ~
        | UOpBitwiseNegation
        /// &
        | UOpAndReduce
        /// ~&
        | UOpNAndReduce
        /// |
        | UOpOrReduce
        /// ~|
        | UOpNOrReduce
        /// ^
        | UOpXOrReduce
        /// ^~ or ~^
        | UOpXNorReduce

    type BinaryOp =
        /// +
        | BOpPlus
        /// -
        | BOpMinus
        /// *
        | BOpStar
        /// /
        | BOpDiv
        /// %
        | BOpMod
        /// ==
        | BOpEquals
        /// !=
        | BOpBangEquals
        /// ===
        | BOpTripleEquals
        /// !==
        | BOpBangTripleEquals
        /// &&
        | BOpLogicalAnd
        /// ||
        | BOpLogicalOr
        /// **
        | BOpExponent
        /// <
        | BOpLessThan
        /// <=
        | BOpLessThanEqual
        /// >
        | BOpGreaterThan
        /// >=
        | BOpGreaterThanEqual
        /// &
        | BOpBitwiseAnd
        /// |
        | BOpBitwiseOr
        /// ~&
        | BOpBitwiseNAnd
        /// ~|
        | BOpBitwiseNOr
        /// ^
        | BOpXor
        /// ^~ or ~^
        | BOpXNor
        /// <<
        | BOpLogicRightShift
        /// >>
        | BOpLogicLeftShift
        /// <<<
        | BOpArithmeticRightShift
        /// >>>
        | BOpArithmeticLeftShift
        
    type Expr =
        | ExprNumber of int
        | ExprConcateneation of Expr list
        | ExprIdentifier of Identifier
        | ExprBinary of Expr * BinaryOp * Expr
        | ExprUnary of UnaryOp * Expr
        | ExprIfThenElse of Expr * Expr * Expr // <expr> ? <expr> : <expr>
        | ExprIndex of Expr * int
        | ExprIndexRange of Expr * int * int

    type ModuleItem =
        | ItemPort of Direction * Identifier
        | ItemAssign of Identifier * Expr
        | ItemWireDecl of Range * Identifier
        | ItemInstantiation of Identifier * Identifier * Expr list

    type Module =
        { name: string
          ports: Identifier list
          items: ModuleItem list }