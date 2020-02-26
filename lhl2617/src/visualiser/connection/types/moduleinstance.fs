(*
    Visualise Module Instance connections
    =====================================

    Author: lhl2617
*)
module Verishot.VisualiseModuleInstanceConnection

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Functions
open Verishot.VisualiseConnectionLib

let visualiseModuleInstanceConnection (elem: ModuleInstance) (nodeMap: NodeMap) labelId = 
    let source = elem.instanceName
    let sourceNode = getNodeFromNodeMap nodeMap source
    
    let targetGroups =
        elem.connections
        |> Map.map (fun sourceId cons -> groupConnections cons)
        |> Map.toList

    let svgElems, finalLabelId = 
        (labelId, targetGroups)
        ||> List.mapFold 
            (fun lId (sourcePort, conGroup) -> 
                let sourceRange = (getPortPropFromVNode sourceNode Output sourcePort).range
                match labelRequired sourceNode sourcePort conGroup nodeMap with 
                | true -> 
                    let sourceSVG = getSourceLabel sourcePort sourceNode sourceRange lId
                    let targetSVG = 
                        conGroup 
                        |> List.map (fun (targetNode, cons) -> getTargetLabelsAndBlobsForNode sourceRange cons (getNodeFromNodeMap nodeMap targetNode) labelId)
                        |> groupSVG [] None
                        
                    [sourceSVG; targetSVG] |> groupSVG [("class", "label-group"); ("id", string lId)] None, lId + 1
                | false -> 
                    let targetNodeId, targets = conGroup |> List.head
                    let targetNode = getNodeFromNodeMap nodeMap targetNodeId
                    [getWiresAndBlobs sourcePort sourceNode sourceRange targets targetNode] |> groupSVG [("class", "label-group"); ("id", string lId)] None, lId
            )
    svgElems |> groupSVG [] None, finalLabelId

let visualiseModuleInstanceConnections (elems: ModuleInstance list) (nodeMap: NodeMap) (labelId: int) =
    let svgElems, finalLabelId = 
        (labelId, elems)
        ||> List.mapFold 
            (fun lId elem -> 
                visualiseModuleInstanceConnection elem nodeMap lId)
    svgElems |> groupSVG [] None, finalLabelId