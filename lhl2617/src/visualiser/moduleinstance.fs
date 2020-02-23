(*
    Visualise Code for `ModuleInstance` type
    ========================================

    Author: lhl2617
*)
module Verishot.VisualiseModuleInstance

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Functions
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.Util

let defaultGraphicsProps =
    {| maxTitleLen = 16
       titleHeight = 2
       maxPortLen = 11 |}

// title
let getTitle (x, y) props instName modName: SVGElement =
    let txy = x + props.marginLeft + props.width / 2., y + (float defaultGraphicsProps.titleHeight) / 2.
    Text
        (txy, truncText instName defaultGraphicsProps.maxTitleLen, [ ("class", "node-mod-title") ],
         Some <| sprintf "Instance: %s\nModule: %s" instName modName)

let truncPortText text len ranged =
    match ranged with
    | true -> truncText text (len - 2) + "[]"
    | false -> truncText text len

(* we also get the list of portProps for use later *)
let getPortTextsAndProps (x, y) props (ports: (Identifier * Range) list) (dir: Direction): (SVGElement * (Identifier * PortProp)) list =
    let uy = y + 2.
    // offset, this gives y for middle of text
    let uyOffset = 0.
    let uVertDist = 1.

    let ux, className, paddedUx =
        match dir with
        | Input -> x + props.marginLeft, "node-mod-leftPortText", x + props.marginLeft + 0.25
        | Output ->
            x + props.marginLeft + props.width, "node-mod-rightPortText", x + props.marginLeft + props.width - 0.25

    let renderPortText = fun i port ->
        let text = fst port

        // range [] or not
        let ranged, range =
            match snd port with
            | Single _ -> false, (0, 0)
            | Range(a, b) -> true, (a, b)

        let truncedText = truncPortText text defaultGraphicsProps.maxPortLen ranged

        let coord = ux, uy + (float i + uVertDist + uyOffset)
        let paddedCoord = paddedUx, snd coord

        let toolTip =
            sprintf "Port: %s" text + 
                match ranged with
                | true ->
                      let rangeA, rangeB = range
                      sprintf "\nRange: %d:%d" rangeA rangeB
                | false -> ""

        Text(paddedCoord, truncedText, [ ("class", className) ], Some toolTip),
        (text,
         { index = i
           coord = coord
           range = snd port })

    ports |> List.mapi renderPortText

let getWidth instName inPorts outPorts =
    match String.length instName >= defaultGraphicsProps.maxTitleLen with
    | true -> 12.
    | _ -> 
        let getMaxPortLength ports =
            match List.isEmpty ports with
            | true -> 0
            | false ->
                ports
                |> List.maxBy (fst >> String.length)
                |> (fst >> String.length)

        let maxPortLen = max (getMaxPortLength inPorts) (getMaxPortLength outPorts) + 2 // leeway for busses
        match maxPortLen >= defaultGraphicsProps.maxPortLen with
        | true -> 12.
        | _ -> 
            max (float maxPortLen + 1.) <| max 4. (float <| String.length instName - 3)

let getHeight inPorts outPorts =
    defaultGraphicsProps.titleHeight + max (List.length inPorts) (List.length outPorts) + 1 |> float

let visualiseDeclaredModuleInstance (elem: ModuleInstance) (modName: string) (declMap: Map<Identifier, ModuleDecl>) (nodeMap: NodeMap) idx xy: NodeMap * int * Coord =
    let decl = getDeclFromDeclMap declMap modName
    
    let inputPorts = decl.ports |> List.filter (fun (x, _, _)-> x = Input) |> List.map (fun (_, x, y) -> (x, y))
    let outputPorts = decl.ports |> List.filter (fun (x, _, _) -> x = Output) |> List.map (fun (_, x, y) -> (x, y))
    
    let props = 
        { defaultModuleInstanceProps with 
            width=getWidth elem.instanceName inputPorts outputPorts
            height=getHeight inputPorts outputPorts
            marginLeft=float <| List.length inputPorts + 5 (* space to let connections bend *) }

    let borderBox = getBorderBox xy props "node-bord"
    let actualBox = getActualBox xy props "node-actual"

    let getPortHelper = getPortTextsAndProps xy props
    let inPortTextsPC = getPortHelper inputPorts Input
    let outPortTextsPC = getPortHelper outputPorts Output

    let gottenInputPortProps = inPortTextsPC |> List.map snd |> Map.ofList 
    let gottenOutputPortProps = outPortTextsPC |> List.map snd |> Map.ofList

    let title = getTitle xy props elem.instanceName modName

    let inPortTexts = inPortTextsPC |> List.map fst |> groupSVG [] None

    let outPortTexts = outPortTextsPC |> List.map fst |> groupSVG [] None

    let svgElem = 
        [borderBox; actualBox; title; inPortTexts; outPortTexts] 
        |> linkSVG (sprintf "%s.svg" modName) [] (Some <| sprintf "Module: %s" modName)

    let visualisedNode =
        { node=ModuleInstance elem
          decl=Some decl
          svg=svgElem
          idx=idx
          coord=xy
          props=
            { props with inputPortProps=gottenInputPortProps
                         outputPortProps=gottenOutputPortProps } }

    let newNodeMap = nodeMap |> (Map.add elem.instanceName visualisedNode)

    let newCoord = fst xy + props.marginLeft + props.marginRight + props.width, snd xy
    newNodeMap, 1 + idx, newCoord

let visualiseBuiltInModuleInstance (arity: int) (elem: ModuleInstance) (nodeMap: NodeMap) idx xy: NodeMap * int * Coord = 
    let inputPorts = 
        match arity with 
        | 1 -> [("in1", Single)]
        | 2 -> [("in1", Single); ("in2", Single)]
        | _ -> failwithf "ERROR: Built in module only allows 1 or 2 inputs; Invalid: %d" arity
        // we won't know if it's a bus, we take as single, operators should be able to 
        // handle anything.
    let outputPorts = ["out", Single]
    
    let props = 
        { defaultModuleInstanceProps with 
            width=getWidth elem.instanceName inputPorts outputPorts
            height=getHeight inputPorts outputPorts }

    let borderBox = getBorderBox xy props "node-builtin-bord"
    let actualBox = getActualBox xy props "node-builtin-actual"

    let getPortHelper = getPortTextsAndProps xy props
    let inPortTextsPC = getPortHelper inputPorts Input
    let outPortTextsPC = getPortHelper outputPorts Output

    let gottenInputPortProps = inPortTextsPC |> List.map snd |> Map.ofList 
    let gottenOutputPortProps = outPortTextsPC |> List.map snd |> Map.ofList

    let title = getTitle xy props elem.instanceName "TODO"

    let inPortTexts = inPortTextsPC |> List.map fst |> groupSVG [] None

    let outPortTexts = outPortTextsPC |> List.map fst |> groupSVG [] None

    let svgElem = 
        [borderBox; actualBox; title; inPortTexts; outPortTexts] 
        |> groupSVG [] (Some <| sprintf "Module: %s" "TODO")

    let visualisedNode =
        { node=ModuleInstance elem
          decl=None
          svg=svgElem
          idx=idx
          coord=xy
          props=
            { props with inputPortProps=gottenInputPortProps
                         outputPortProps=gottenOutputPortProps } }

    let newNodeMap = nodeMap |> (Map.add elem.instanceName visualisedNode)

    let newCoord = fst xy + props.marginLeft + props.marginRight + props.width, snd xy
    newNodeMap, 1 + idx, newCoord

let visualiseModuleInstance (elem: ModuleInstance) (declMap: Map<Identifier, ModuleDecl>) (nm: NodeMap) idx xy: NodeMap * int * Coord =
    let f = 
        match elem.moduleName with 
        | StringIdentifier modName -> 
            visualiseDeclaredModuleInstance elem modName declMap
        | BOpIdentifier _ -> 
            visualiseBuiltInModuleInstance 2 elem
        | UOpIdentifier _ -> 
            visualiseBuiltInModuleInstance 1 elem
    f nm idx xy

let visualiseModuleInstances (elems: ModuleInstance list) (declMap: Map<Identifier, ModuleDecl>) 
    nodeMap (index: int) (coord: Coord) =
    
    ((nodeMap, index, coord), elems)
    ||> List.fold
            (fun (nm, idx, xy) elem ->
                visualiseModuleInstance elem declMap nm idx xy)

