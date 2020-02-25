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
open Verishot.VisualiserUtil.Common
open Verishot.VisualiserUtil.Functions

let defaultGraphicsProps = 
    {| maxConnectionTextLen=12
       xSize=0.25
       varTextOffset=0.2
       varTextTransformUp=0.1
       labelTextOffset=0.5 |}

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
        let tmp =
            if List.head targetYs <> sourceY then targetYs.[1..] else targetYs

        let blobYs =
            if List.last targetYs <> sourceY then tmp.[..tmp.Length - 2] else tmp

        blobYs |> List.map (fun cy -> Circle((bendPointX, cy), commonGraphicsProps.blobRadius, [ ("class", "wire-blob") ], None))

let groupConnections (cons: Connection list) (sourceRange: Range): ConnectionGroup =
    (* group connections by targetNodes and portNames 
        List<TargetNodeId * (TargetPortName * Range)>

        we take in sourceIsBus to know if the input is Single or Range
    *)
    let sourceIsBus = 
        match sourceRange with 
        | Range _ -> true
        | _ -> false

    (* WARNING: This won't support concatenated busses. *)
    let getRangeFromList (x: int list) =
        match sourceIsBus with
        | false -> Single
        | _ -> 
            let min = List.min x
            let max = List.max x
            Range (max, min)

    let flattenPortnameGroup =
        fun (portName: Identifier, con: List<Identifier * (int * int)>) ->
            portName,
            con
            |> List.map snd
            |> List.unzip
            |> fun (x, y) -> { inputRange=getRangeFromList x; outputRange=getRangeFromList y }

    let getTargetNodeId (con: Connection) =
        match con.target with 
        | PinTarget x -> x.pinName
        | InstanceTarget x -> x.targetNode  
    
    let groupTargetConnections (con: Connection) =
        match con.target with 
        | PinTarget x -> x.pinName, (con.srcPortIndex, x.pinIndex)
        | InstanceTarget x -> x.portName, (con.srcPortIndex, x.portIndex)
    
    cons
    |> List.groupBy getTargetNodeId
    |> List.map (fun (id, cons) ->
        id,
        cons
        |> List.map groupTargetConnections
        |> List.groupBy fst
        |> List.map flattenPortnameGroup
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
            (fun (targetPortId, connectionRanges) -> 
                let sourceRange = connectionRanges.inputRange
                let endpointProp = getPortPropFromVNode targetNode Input targetPortId 
                let pt4 = endpointProp.coord
                let pt3 = fst pt2, snd pt4
                
                let varStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen sourceRange
                
                let line = Polyline([pt2; pt3; pt4], [("class", getLineClassName sourceRange)], None)
                let varText = Text((fst pt4 - defaultGraphicsProps.varTextOffset, snd pt4 - defaultGraphicsProps.varTextTransformUp), varStr, [("class", "input-var-text")], Some <| sprintf "Bus: %s" varStr)

                [line; varText] |> groupSVG [] None)
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

    distance between *3 and *4 is (number of inports of RB - index of last inport of RB)
    taking the last port is just for aesthetic purposes to make labels more readable.
    This means that the messy parts are closer to the RB, allowing more whitespace for labels
    closer to the margin
    *)

    // draw pt1 and pt2 first
    let firstCon = List.head targets
    let firstEndpointProp = getPortPropFromVNode targetNode Input (fst firstCon)
    let lastCon = List.last targets
    let lastEndpointProp = getPortPropFromVNode targetNode Input (fst lastCon)
    
    let firstCoord = firstEndpointProp.coord
    let lastTargetIdx = lastEndpointProp.index

    let numberOfInputs = getNumberOfInputsFromVNode targetNode

    let bendPointOffset = numberOfInputs - lastTargetIdx

    let bendPointX = fst firstCoord - float bendPointOffset - 2.

    let labelStr = "l" + string labelId
    
    let pt1 = fst firstCoord - targetNode.props.marginLeft + 2., snd firstCoord
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

    distance between *3 and *4 is number of inports of RB - index of last inport of RB
    taking the last port is just for aesthetic purposes to make labels more readable.
    This means that the messy parts are closer to the RB, allowing more whitespace for labels
    closer to the margin
    *)
    let sourceProps = getPortPropFromPortProps sourceNode.props.outputPortProps source
    let pt1 = sourceProps.coord

    let lastTargetEndpointProp = 
        targets 
        |> List.last 
        |> fst 
        |> getPortPropFromPortProps targetNode.props.inputPortProps

    let numberOfInputs = getNumberOfInputsFromVNode targetNode

    let bendPointOffset = numberOfInputs - lastTargetEndpointProp.index
    
    let bendPointX = fst lastTargetEndpointProp.coord - (float bendPointOffset) - 2.
    
    let pt2 = bendPointX, snd pt1
    let varStr = truncConnectionText "" defaultGraphicsProps.maxConnectionTextLen sourcePortRange
   
    // draw line from pt1 to pt2 first
    let sourceLine = Polyline([pt1; pt2], [("class", getLineClassName sourcePortRange)], None)
    let varText = Text((fst pt1 + defaultGraphicsProps.varTextOffset, snd pt1 - defaultGraphicsProps.varTextTransformUp), varStr, [("class", "output-var-text")], Some <| sprintf "Bus: %s" varStr)
    
    let sourceSVG = [sourceLine; varText] |> groupSVG [] None

    let linesSVG, blobsSVG = getLinesAndBlobsToTarget targets targetNode pt2
        
    [sourceSVG; linesSVG; blobsSVG] |> groupSVG [] None