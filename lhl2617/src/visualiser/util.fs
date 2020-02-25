namespace Verishot.VisualiserUtil

open System.Web
open Verishot.Util
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.CoreTypes.VerilogAST
open Verishot.SVG
open Verishot.VisualiseMaps

type PortProp =
    { index: int
      coord: Coord
      range: Range } // absolute location of an endpoint

type NodeProps =
    { width: float
      height: float
      marginLeft: float
      marginRight: float
      inputPortProps: Map<Identifier, PortProp> 
      outputPortProps: Map<Identifier, PortProp> } 

type VisualisedNode =
    { node: Node
      decl: ModuleDecl Option // if it's a module, have the decl
      svg: SVGElement
      idx: int
      coord: Coord // top left
      props: NodeProps }

type NodeMap = Map<Identifier, VisualisedNode>

module ModuleInstance =
    let defaultModuleInstanceProps =
        { width = 0. // determined later
          height = 0. // determined later
          marginLeft = 6.
          marginRight = 6.
          inputPortProps = Map.empty // determined later
          outputPortProps = Map.empty }  // determined later

module Pin =
    let defaultPinProps =
        { width = 12.
          height = 1.
          marginLeft = 0. // determined later
          marginRight = 0. // determined later
          inputPortProps = Map.empty // determined later
          outputPortProps = Map.empty } // determined later

    let defaultPinMargin = 8.
     
module Connection =
    type ConnectionRange = 
        { inputRange: Range
          outputRange: Range }
  
    type ConnectionGroup = 
        (Identifier * ((Identifier * ConnectionRange) list)) list
        // List<TargetNodeId * (TargetPortName * (InputRange * OutputRange))>

module Functions =
    let truncText text len =
        // if len < 3 but textlen > len, the output is "..."
        match String.length text <= len with
        | true -> text
        | _ -> text.[0..len-4] + "..."

    let getRangeStr (range: Range) = 
        match range with
        | Range (a, b) -> 
            match a = b with 
            | true -> sprintf "[%d]" a
            | _ -> sprintf "[%d:%d]" a b
        | _ -> ""

    let getNumberOfInputsFromNode (node: Node) =
        match node with
        | ModuleInstance modInst -> 
            match modInst.moduleName with 
            | UOpIdentifier _ -> 1
            | BOpIdentifier _ -> 2
            | _ -> failwithf "ERROR: Unable to get number of inputs of module '%A'" modInst.moduleName
        | OutputPin _ -> 1
        | _ -> 0
        
    let getNumberOfInputsFromVNode (targetNode: VisualisedNode) = 
        match targetNode.decl with
        | Some decl -> 
            decl.ports
            |> List.filter (fun (x, _, _) -> x = Input)
            |> List.length
        | _ -> 
            getNumberOfInputsFromNode targetNode.node
            

    // border
    let getBorderBox xy props className: SVGElement =
        let bwh = props.marginLeft + props.marginRight + props.width, props.height

        Rectangle(xy, bwh, [ ("class", className) ], None)

    // actual
    let getActualBox (x, y) props className: SVGElement =
        let bxy = x + props.marginLeft, y
        let wh = props.width, props.height

        Rectangle(bxy, wh, [ ("class", className) ], None)

    let loadCSSWithUnit (path: string) (unitPx: float): string =
        let cssString = readFileToString path
        let preamble = sprintf ":root {\n\t--unitPx: %.1fpx\n}\n" unitPx
        preamble + cssString
        
    let getSVGFromNodeMap nodeMap props = 
        nodeMap
        |> Map.toList
        |> List.map (snd >> fun x -> x.svg)
        |> groupSVG props None

    let getNodeFromNodeMap (nodeMap: NodeMap) id =
        match Map.containsKey id nodeMap with
        | true -> nodeMap.[id]
        | false -> failwithf "ERROR: Node '%s' not found." id
     
    let getDeclFromDeclMap (declMap: Map<Identifier, ModuleDecl>) moduleName =
        match Map.containsKey moduleName declMap with
        | true -> declMap.[moduleName]
        | false -> failwithf "ERROR: Module declaration '%s' does not exist" moduleName

    let getPortFromModuleDecl (decl: ModuleDecl) (portId: Identifier) =
        match List.tryFind ((fun (_, x, _) -> x) >> (=) portId) decl.ports with
        | Some x -> x
        | _ -> failwithf "ERROR: Port '%s' not found on module '%s'" portId decl.name

    let getPortPropFromPortProps (portProps: Map<Identifier, PortProp>) (id: Identifier) =
        match Map.containsKey id portProps with
        | true -> portProps.[id]
        | false -> failwithf "ERROR: Port '%s' not found." id

    let getPortPropFromVNode (vNode: VisualisedNode) (dir: Direction) (id: Identifier) =
        let portProps = 
            match dir with
            | Input -> vNode.props.inputPortProps
            | Output -> vNode.props.outputPortProps
        getPortPropFromPortProps portProps id

    let getBinaryOpHTMLString (op: BinaryOp) = 
        match Map.tryFind op BinaryOpMap with 
        | Some ret -> HttpUtility.HtmlEncode ret
        | _ -> failwithf "ERROR: Binary operator '%A' does not exist." op

    let getUnaryOpHTMLString (op: UnaryOp) =
        match Map.tryFind op UnaryOpMap with 
        | Some ret -> HttpUtility.HtmlEncode ret
        | _ -> failwithf "ERROR: Unary operator '%A' does not exist." op
