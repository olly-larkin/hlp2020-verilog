(*
    Visualise library code for `Connection` type
    ============================================

    Author: lhl2617
*)

module Verishot.VisualiseConnectionLib

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Connection
open Verishot.VisualiserUtil.Functions

let defaultGraphicsProps = 
    {| maxConnectionTextLen=12
       xSize=0.25
       varTextOffset=0.1
       varTextTransformUp=0.1
       labelTextOffset=0.5
       blobRadius=0.15 |}

// get the X graphic
let getLabelX (x, y) =
    let offset = defaultGraphicsProps.xSize

    let topLeft = x - offset, y - offset
    let btmRight = x + offset, y + offset

    let btmLeft = x - offset, y + offset
    let topRight = x + offset, y - offset

    Group
        ([ Polyline([ topLeft; btmRight ], [], None)
           Polyline([ btmLeft; topRight ], [], None) ], [ ("class", "label-x") ], None)

let getBlobs sourceY bendPointX targetYs =
    (*
    add blobs
    LB: Left block
    RB: Right block

    this works for label blobs too

    Add the blobs at points #
      LB    margin    bendPointX      RB
    -----     |          ^      -----
    |   |----------------+      |   |   -sourceY
    |   |                #------|   |   }\
    |   |                #------|   |   } > targetYs
    |   |                +------|   |   }/
    -----                       -----

    NB if the last or the first node has y same as source, then blob needed
    otherwise, we don't blob it
    *)
    match List.length targetYs <= 1 with
    | true -> []
    | _ -> 
        let firstIdx = if List.head targetYs <> sourceY then 1 else 0
        let lastIdx  = if List.last targetYs <> sourceY then List.length targetYs - 2 else List.length targetYs - 1

        let blobYs = targetYs.[firstIdx..lastIdx]

        blobYs |> List.map (fun cy -> Circle((bendPointX, cy), defaultGraphicsProps.blobRadius, [ ("class", "wire-blob") ], None))

let groupConnections (cons: Connection list): ConnectionGroup =
    (* group connections by targetNodes and portNames 
        / List<TargetNodeId * (TargetPortName * List<InputRange * OutputRange>)>
    *)
    let getTargetNodeId (con: Connection) =
        match con.target with 
        | PinTarget x -> x
        | InstanceTarget (x, _) -> x  
    
    let groupByTarget (con: Connection): Identifier * ConnectionRange =
        let conRange = { inputRange=con.srcRange; outputRange=con.targetRange }
        match con.target with  
        | PinTarget pinName -> pinName, conRange
        | InstanceTarget (_, portName) -> portName, conRange

    cons
    |> List.groupBy getTargetNodeId
    |> List.map (fun (id, cons) ->
        id,
        cons
        |> List.map groupByTarget
    )

let truncConnectionText text len range = 
    let rangeStr = getRangeStr range
    truncText text (len - String.length rangeStr) + rangeStr

let getLineClassName range =
    match range with 
    | Single -> "label-line-single"
    | Range (min, max) when min = max -> "label-line-single"
    | _ -> "label-line-range"
        
let getSourceLabel (sourcePort: Identifier) (sourceNode: VisualisedNode) (range: Range) (labelId: int) =
    let endpointProp = getPortPropFromVNode sourceNode Output sourcePort 

    let lCoord = endpointProp.coord
    let rCoord = fst lCoord + sourceNode.props.marginRight - 2. , snd lCoord
    
    let labelStr = "l" + string labelId
    let varStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen range 
    
    let labelLine = Polyline([lCoord; rCoord], [("class", getLineClassName range)], Some <| sprintf "Label: %s" labelStr)
    let labelX = getLabelX rCoord
    let labelText = Text((fst rCoord + defaultGraphicsProps.labelTextOffset, snd rCoord), labelStr, [("class", "label-output-text")], Some <| sprintf "Label: %s" labelStr)
    let varText = Text((fst lCoord + defaultGraphicsProps.varTextOffset, snd lCoord - defaultGraphicsProps.varTextTransformUp), varStr, [("class", "output-var-text")], Some <| sprintf "Bus: %s" varStr)

    [labelLine; labelX; labelText; varText] |> groupSVG [] None

let getLinesAndBlobsToTarget (targets: (Identifier * ConnectionRange) list) (targetNode: VisualisedNode) pt2 =
    (*
    Helper func to draw everything between *2 and *4

    *1     *2      -----
    X------+-------|   |
           +-------|   |
           |       |   |
           |     *4|   |
           +-------|   |
           *3      -----

    *)
    let linesSVG = 
        targets 
        |> List.map 
            (fun (targetPortId, conRange) -> 
                let sourceRange = conRange.inputRange
                let targetRange = conRange.outputRange

                let endpointProp = getPortPropFromVNode targetNode Input targetPortId 
                let pt4 = endpointProp.coord
                let pt3 = fst pt2, snd pt4

                let sourceStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen sourceRange
                let targetStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen targetRange
                
                let line = Polyline([pt2; pt3; pt4], [("class", getLineClassName sourceRange)], None)

                let textY = snd pt4 - defaultGraphicsProps.varTextTransformUp

                let sourceText = Text((fst pt3 + defaultGraphicsProps.varTextOffset, textY), sourceStr, [("class", "input-var-text-source")], Some <| sprintf "Source Bus: %s" sourceStr)
                let targetText = Text((fst pt4 - defaultGraphicsProps.varTextOffset, textY), targetStr, [("class", "input-var-text-target")], Some <| sprintf "Target Bus: %s" targetStr)

                [line; sourceText; targetText] |> groupSVG [] None)
        |> groupSVG [] None

    let targetYs = 
        targets
        |> List.map 
            (fun (targetPortId, _) -> 
                targetPortId 
                |> getPortPropFromVNode targetNode Input
                |> fun x -> x.coord
                |> snd)

    let blobsSVG = getBlobs (snd pt2) (fst pt2) targetYs |> groupSVG [("class", "wire-blob-group")] None
    
    linesSVG, blobsSVG

let getTargetLabelsAndBlobsForNode (sourceRange: Range) (targets: (Identifier * ConnectionRange) list) (targetNode: VisualisedNode) labelId =
    (*
    targets: List of portnames

    BP: bendpoint
    *n: pt(n)
          BP        
    *1     *2      -----
    X------+-------|   |
           +-------|   |
           |       |   |
           |     *4|   |
           +-------|   |
           *3      -----

    distance between *1 and *2 is the index of the first port + 1.
    *)

    // draw pt1 and pt2 first
    let firstCon = List.head targets
    let firstEndpointProp = getPortPropFromVNode targetNode Input (fst firstCon)
    
    let firstCoord = firstEndpointProp.coord
    let firstIdx = firstEndpointProp.index

    let labelStr = "l" + string labelId
    
    let pt1 = fst firstCoord - targetNode.props.marginLeft + 2., snd firstCoord
    let bendPointX = fst pt1 + float firstIdx + 1.
    let pt2 = bendPointX, snd pt1

    let labelLine = Polyline([pt1; pt2], [("class", getLineClassName sourceRange)], Some <| "Label: " + labelStr)
    let labelX = getLabelX pt1
    let labelText = Text((fst pt1 - defaultGraphicsProps.labelTextOffset, snd pt1), labelStr, [("class", "label-input-text")], Some <| sprintf "Label: %s" labelStr)
    
    let labelSVG = [labelLine; labelX; labelText] |> groupSVG [] None
    
    let linesSVG, blobsSVG = getLinesAndBlobsToTarget targets targetNode pt2

    [labelSVG; linesSVG; blobsSVG] |> groupSVG [] None

let labelRequired (sourceNode: VisualisedNode) (sourcePortId: Identifier) (conGroup: ConnectionGroup) (nodeMap: NodeMap): bool = 
    (* true if label required 
        Label not required if:
            targetNode is next node only &&
            y Coord of source port is contained by one of the y's of inputs of targetNode
    *)

    match List.length conGroup with 
    | 1 ->
        let con = List.head conGroup

        let targetNode = 
            con 
            |> fst 
            |> getNodeFromNodeMap nodeMap
        
        let targetNodeIdx = targetNode.idx

        let targetNodeIsNextNodeOnly = 
            targetNodeIdx = sourceNode.idx + 1

        let targetNodeIsModuleInstance = 
            match targetNode.node with 
            | ModuleInstance _ -> true
            | _ -> false

        match targetNodeIsNextNodeOnly && targetNodeIsModuleInstance with
        | true ->
            // now we have to check whether targetYs contain sourceY
            let endpointProps = targetNode.props.inputPortProps

            let targetYs = 
                con 
                |> snd
                |> List.map 
                    (fst
                     >> getPortPropFromPortProps endpointProps 
                     >> fun x -> snd x.coord
                    )
                    
            let sourcePortY = 
                sourcePortId
                |> getPortPropFromPortProps sourceNode.props.outputPortProps
                |> fun x -> snd x.coord

            not <| List.contains sourcePortY targetYs
        | false -> true
    | _ -> true


let getWiresAndBlobs (source: Identifier) (sourceNode: VisualisedNode) (sourcePortRange: Range) 
    (targets: (Identifier * ConnectionRange) list) (targetNode: VisualisedNode) =
    (*
    LB: Left block
    RB: Right block
    BP1: Bend-point 1
    BP2: Bend-point 2
    *n: pt(n)

      LB  margin   BP1        RB
    -----   ^       ^*2     -----
    |   |---|-------+-------|   |
    |   |*1         +-------|   |
    |   |           |       |   |
    |   |           |     *4|   |
    |   |         *3+-------|   |
    -----          BP2      -----


    distance between margin and *2 is the index of the first port + 1.
    *)
    let sourceProps = getPortPropFromPortProps sourceNode.props.outputPortProps source
    let pt1 = sourceProps.coord

    let firstTargetEndpointProp = 
        targets 
        |> List.head 
        |> fst 
        |> getPortPropFromPortProps targetNode.props.inputPortProps

    let bendPointX = fst pt1 + sourceNode.props.marginRight + float firstTargetEndpointProp.index + 1.
    
    let pt2 = bendPointX, snd pt1
    let varStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen sourcePortRange
   
    // draw line from pt1 to pt2 first
    let sourceLine = Polyline([pt1; pt2], [("class", getLineClassName sourcePortRange)], None)
    let varText = Text((fst pt1 + defaultGraphicsProps.varTextOffset, snd pt1 - defaultGraphicsProps.varTextTransformUp), varStr, [("class", "output-var-text")], Some <| sprintf "Bus: %s" varStr)
    
    let sourceSVG = [sourceLine; varText] |> groupSVG [] None

    let linesSVG, blobsSVG = getLinesAndBlobsToTarget targets targetNode pt2
        
    [sourceSVG; linesSVG; blobsSVG] |> groupSVG [] None

let visualiseLabel sourceNode sourcePort sourceRange (conGroup: ConnectionGroup) nodeMap lId =
    let sourceSVG = getSourceLabel sourcePort sourceNode sourceRange lId
    let targetSVG = 
        conGroup 
        |> List.map (fun (targetNode, cons) -> getTargetLabelsAndBlobsForNode sourceRange cons (getNodeFromNodeMap nodeMap targetNode) lId)
        |> groupSVG [] None
        
    [sourceSVG; targetSVG] |> groupSVG [("class", "label-group"); ("id", string lId)] None, lId + 1

let visualiseWire sourceNode sourcePort sourceRange (conGroup: ConnectionGroup) nodeMap (lId: int) = 
    let targetNodeId, targets = conGroup |> List.head
    let targetNode = getNodeFromNodeMap nodeMap targetNodeId

    [getWiresAndBlobs sourcePort sourceNode sourceRange targets targetNode] |> groupSVG [("class", "label-group"); ("id", string lId)] None, lId