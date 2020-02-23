(*
    Visualise Code for `InputPin` & `OutputPin` type
    ================================================

    Author: lhl2617
*)

module Verishot.VisualisePin

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Pin
open Verishot.VisualiserUtil.Functions
open Verishot.Util

let defaultGraphicsProps =
    {| maxTitleLen=16 |}

let getArrow (x, y) props dir =
    (* 
    given xy is top left of whole border
    
    *n: pt(n)

    INPUT
    *1
    ----- *2
    |    > *3
    ----- *4
    *5   

    OUTPUT
     *1
     -----*2
  *5<    |
     -----*3
     *4

     |<--- 8u --->|<-4u->|
     ---------------------
     |                   | > 0.25u 
     |            ======\|
     |            ======/| 
     |                   |
     ---------------------
    *)
    

    let pts = 
        match dir with 
        | Input -> 
            let pt1 = x + 8., y + 0.25
            let pt2 = fst pt1 + 3.5, snd pt1
            let pt3 = fst pt2 + 0.5, snd pt2 + 0.25
            let pt4 = fst pt2, snd pt2 + 0.5
            let pt5 = fst pt1, snd pt1 + 0.5
            [pt1; pt2; pt3; pt4; pt5; pt1]
        | Output -> 
            let pt1 = x + props.width - 3.5, y + 0.25
            let pt2 = fst pt1 + 3.5, snd pt1
            let pt3 = fst pt2, snd pt2 + 0.5
            let pt4 = fst pt1, snd pt1 + 0.5
            let pt5 = fst pt4 - 0.5, snd pt4 - 0.25
            [pt1; pt2; pt3; pt4; pt5; pt1]

    Polyline (pts, [("class", "pin-input-arrow")], "")

let truncPinText (text: Identifier) len (rangeStr: string) =
    truncText text (len - String.length rangeStr) + rangeStr

let getTitle (x, y) text dir range props =
    let txy, className =
        match dir with
        | Input -> 
            (x + 0.25, y + defaultPinProps.height / 2.), 
            "pin-text-input"
        | Output -> 
            (x + props.width + props.marginLeft - 0.25, y + defaultPinProps.height / 2.),
            "pin-text-output"

    let truncedText, toolTip =
        let rangeStr = getRangeStr range
        let longRangeStr = 
            match String.length rangeStr with
            | 0 -> ""
            | _ -> sprintf "\nRange: %s" rangeStr
        let pinStr = 
            match dir with 
            | Input -> sprintf "Input Pin: %s%s" text longRangeStr
            | Output -> sprintf "Output Pin: %s%s" text longRangeStr
        truncPinText text defaultGraphicsProps.maxTitleLen rangeStr, pinStr 

    Text (txy, truncedText, [("class", className)], toolTip)

let getPortProp (x, y) width dir range =
    match dir with 
    | Input -> { index=0; coord=(x + width, y + defaultPinProps.height / 2.); range=range }
    | Output -> { index=0; coord=(x + defaultPinMargin, y + defaultPinProps.height / 2.); range=range }

let visualisePin (currDecl: ModuleDecl) (elem: Identifier) (nodeMap: NodeMap) idx xy dir = 
    (*
      INPUT
      ..........(8u)......(6u)
    . --------------------- 
    . | id          ======>| (8u marginRight)
    . --------------------- 

      OUTPUT
    .                  --------------------- 
    . (8u) marginLeft) |<======         id | 
    .                  --------------------- 
    *)   
    let _, _, range = getPortFromModuleDecl currDecl elem

    let props = 
        let portProps = Map [(elem, getPortProp xy defaultPinProps.width dir range)]
        match dir with 
        | Input -> 
            { defaultPinProps with 
                marginRight=defaultPinMargin
                outputPortProps=portProps }
        | Output -> 
            { defaultPinProps with 
                marginLeft=defaultPinMargin
                inputPortProps=portProps }
   
    let borderBox = getBorderBox xy props "node-bord"
    let actualBox = getActualBox xy props "inputPin-actual"
    let title = getTitle xy elem dir range props
    let arrow = getArrow xy props dir

    let svgElem = Group ([borderBox; actualBox; title; arrow], [], "")

    let node = 
        match dir with
        | Input -> InputPin (elem, []) // empty connections is fine, we never need to use it
        | Output -> OutputPin elem 
        
    let visualisedNode =
        { node=node
          decl=None
          svg=svgElem
          idx=idx
          coord=xy
          props=props }

    let newNodeMap = nodeMap |> (Map.add elem visualisedNode)

    let newCoord = fst xy, snd xy + props.height

    newNodeMap, idx + 1, newCoord

let visualisePins currDecl elems dir nodeMap index coord =
    ((nodeMap, index, coord), elems)
    ||> List.fold 
            (fun (nm, idx, xy) elem ->
                visualisePin currDecl elem nm idx xy dir)
    