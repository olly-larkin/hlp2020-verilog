(*
    Visualise Code for `Constant` type
    ==================================

    Author: lhl2617
*)

module Verishot.VisualiseConstant

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Functions
open Verishot.Util

type ConstantElem = {| connections: Connection list; value: int; width: int |}

let defaultGraphicsProps = 
    {| diamondOffset = 0.25
       varTextOffset=0.2
       varTextTransformUp=0.1 |}

let verifyConstantConnections (cons: Connection list) = 
    let groupTargets =
        fun target -> 
            match target with 
            | PinTarget t -> t, t
            | InstanceTarget (targetId, portId) -> targetId, portId

    let extractTarget = fun con -> con.target
    
    let targets = 
        cons 
        |> List.map (extractTarget >> groupTargets)

    let targetNode = 
        targets
        |> List.groupBy fst
        |> List.map fst
        |> List.distinct

    let portName =
        targets
        |> List.groupBy snd
        |> List.map fst
        |> List.distinct
    
    match List.length targetNode = List.length portName && List.length targetNode = 1 with
    | true ->
        List.head targets
    | _ -> failwithf "ERROR: Constants can only connect to one Pin or Module Instance."

let getDiamond (x, y) = 
    let offset = defaultGraphicsProps.diamondOffset
    let top = (x, y - offset)
    let right = (x + offset, y)
    let btm = (x, y + offset)
    let left = (x - offset, y)

    Polyline ([top; right; btm; left; top], ["class", "label-diamond"], None)


let visualiseConstant (nodeMap: NodeMap) (elem: ConstantElem): SVGElement =
    let value, width, cons = elem.value, elem.width, elem.connections    
    let targetNodeId, portId = verifyConstantConnections cons 
    let targetNode = getNodeFromNodeMap nodeMap targetNodeId

    (*
    marginLeft                [const]
            |  (2u)  o---------------|
                     ^pt1            ^pt2
    *)              

    let binStr = string value

    let endpointProp = getPortPropFromVNode targetNode Input portId 
    let pt2 = endpointProp.coord
    let pt1 = fst pt2 - targetNode.props.marginLeft + 2., snd pt2

    let diamond = getDiamond pt1 
    let line = Polyline ([pt1; pt2], [], None)
    let text = Text((fst pt2 - defaultGraphicsProps.varTextOffset, snd pt2 - defaultGraphicsProps.varTextTransformUp), 
                binStr, [("class", "const-text")], Some <| sprintf "Constant: %d\nWidth: %d" value width)
    
    [diamond; line; text] |> groupSVG [("class", "const")] None


let visualiseConstants (elems: ConstantElem list) (nodeMap: NodeMap): SVGElement =
    elems 
    |> List.map (visualiseConstant nodeMap)
    |> groupSVG [("class", "const-group")] None
