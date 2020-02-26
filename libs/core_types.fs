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
        | PinTarget of pinName: Identifier
        | InstanceTarget of targetNode: Identifier * portName: Identifier

    type Connection =
        { srcRange: Range
          targetRange: Range
          target: ConnectionTarget }

    /// Instance of a module.
    [<CustomEquality>]
    [<NoComparison>]
    type ModuleInstance =
        { /// Name of the module being declared (first identifier in verilog declaration)
          moduleName: ModuleIdentifier

          /// Name of the instance (second identifier in verilog declaration)
          instanceName: Identifier

          /// *Outgoing* connections of the module
          connections: Map<Identifier, Connection list> }

        override this.GetHashCode() = (this.moduleName, this.instanceName, this.connections).GetHashCode()

        override this.Equals(thatObj: obj) =
            match thatObj with
            | :? ModuleInstance as that ->
                List.reduce (&&)
                    [ this.moduleName = that.moduleName
                      this.instanceName = that.instanceName
                      equivalentMaps sameListElements this.connections that.connections ]
            | _ -> false


    [<CustomEquality>]
    [<NoComparison>]
    type Node =
        /// An input pin of the *module the netlist refers to*
        | InputPin of Identifier * Connection list
        /// An output pin of the *module the netlist refers to*
        | OutputPin of Identifier
        | ModuleInstance of ModuleInstance
        | Constant of {| value: int; width: int; connections: Connection list |}

        override this.GetHashCode() =
            match this with
            | InputPin(name, conns) -> (1, name, conns).GetHashCode()
            | OutputPin(name) -> (2, name).GetHashCode()
            | ModuleInstance instance -> (3, instance).GetHashCode()
            | Constant constant -> (4, constant).GetHashCode()

        override this.Equals(thatObj: obj) =
            match thatObj with
            | :? Node as that ->
                match this, that with
                | InputPin(thisName, theseConns), InputPin(thatName, thoseConns) ->
                    thisName = thatName && sameListElements theseConns thoseConns
                | OutputPin(thisName), OutputPin(thatName) -> thisName = thatName
                | ModuleInstance(thisInstance), ModuleInstance(thatInstance) -> thisInstance = thatInstance
                | Constant thisConstant, Constant thatConstant ->
                    List.reduce (&&)
                        [ thisConstant.value = thatConstant.value
                          thisConstant.width = thatConstant.width
                          sameListElements thisConstant.connections thatConstant.connections ]
                | _, _ -> false
            | _ -> false

    type Netlist =
        { nodes: Node list
          moduleName: Identifier }

    /// Check that 2 lists have the same elements, irrespective of order.
    /// Used for equality comparisons. It must be defined here, not in Util,
    /// because Util imports this module
    let private sameListElements lst1 lst2 =
        List.forall (fun el -> List.contains el lst2) lst1 && List.forall (fun el -> List.contains el lst1) lst2

    /// Check that 2 maps have equivalent elements, with equivalence given by some function
    /// Used for equality comparisons. It must be defined here, not in Util,
    /// because Util imports this module
    let private equivalentMaps equiv map1 map2 =
        let holds1to2 =
            map1
            |> Map.forall (fun name conn ->
                map2
                |> Map.tryFind name
                |> function
                | Some otherConns -> sameListElements conn otherConns
                | None -> false)

        let holds2to1 =
            map2
            |> Map.forall (fun name conn ->
                map1
                |> Map.tryFind name
                |> function
                | Some otherConns -> sameListElements conn otherConns
                | None -> false)

        holds1to2 && holds2to1

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

    type Index =
        | IndexNum of int
        | IndexRange of int * int
        
    type Expr =
        | ExprNumber of
            width : int option *
            value : int
        | ExprConcateneation of Expr list
        | ExprIdentifier of Identifier
        | ExprBinary of Expr * BinaryOp * Expr
        | ExprUnary of UnaryOp * Expr
        | ExprIfThenElse of Expr * Expr * Expr // <expr> ? <expr> : <expr>
        | ExprIndex of Expr * Index

    type ModuleItem =
        | ItemPort of Direction * Range * Identifier
        | ItemAssign of Identifier * Expr
        | ItemWireDecl of Range * Identifier
        | ItemInstantiation of Identifier * Identifier * Expr list

    type Module =
        { name: string
          ports: Identifier list
          items: ModuleItem list }
