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
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.Util

type Constant = int * int * (Connection list)

let defaultGraphicsProps = {| diamondOffset = 0.25 |}

let verifyConstantConnections (cons: Connection list) = 
    (* true if pass tests *)
    let groupTargets =
        fun target -> 
            match target with 
            | PinTarget t -> t.pinName, t.pinName
            | InstanceTarget t -> t.targetNode, t.portName

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

let rec intToBinary i = 
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBinary (i / 2)) + bit

let getLabelDiamond (x, y) = 
    let o = defaultGraphicsProps.diamondOffset
    let top = (x, y - o)
    let right = (x + o, y)
    let btm = (x, y + o)
    let left = (x - o, y)

    Polyline ([top; right; btm; left; top], ["class", "label-diamond"], None)


let visualiseConstant (value, width, cons) (nodeMap: NodeMap): SVGElement =
    let targetNodeId, portId = verifyConstantConnections cons 
    let targetNode = getNodeFromNodeMap nodeMap targetNodeId

    let binStr = intToBinary value


    

    failwith "TODO"

let visualiseConstants (elems: Constant list) (nodeMap: NodeMap): NodeMap =
    
    failwith "TODO"