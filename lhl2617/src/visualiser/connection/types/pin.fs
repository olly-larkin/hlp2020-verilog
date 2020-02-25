(*
    Visualise Pin connections
    =========================

    Author: lhl2617
*)
module Verishot.VisualisePinConnection

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Functions
open Verishot.VisualiseConnectionLib

let visualiseInputPinConnection (nodeMap: NodeMap) (source: Identifier) (cons: Connection list) labelId =
    // we don't draw anything if cons is empty
    match List.length cons with 
    | 0 -> 
        [] |> groupSVG [] None, labelId
    | _ -> 
        let sourceNode = getNodeFromNodeMap nodeMap source

        (* for pins we always use labels, much easier and less likely to fail *)
        let sourceRange = (getPortPropFromVNode sourceNode Output source).range
        let conGroup = groupConnections cons sourceRange
        
        let sourceSVG = getSourceLabel source sourceNode sourceRange labelId
        let targetSVG = 
            conGroup 
            |> List.map (fun (targetNode, cons) -> getTargetLabelsAndBlobsForNode sourceRange cons (getNodeFromNodeMap nodeMap targetNode) labelId)
            |> groupSVG [] None

        [sourceSVG; targetSVG] |> groupSVG [("class", "label-group")] None, labelId + 1

let visualiseInputPinConnections (elems: (Identifier * Connection list) list) (nodeMap: NodeMap) (labelId: int) =
    (labelId, elems)
    ||> List.mapFold (fun lId (src, cons) -> visualiseInputPinConnection nodeMap src cons lId)
    |> (fun (svgs, lId) -> svgs |> groupSVG [] None, lId)
