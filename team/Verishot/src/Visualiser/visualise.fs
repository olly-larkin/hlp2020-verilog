(*
    Visualise code (entry point)
    ============================

    Author: lhl2617
*)

module Verishot.Visualise

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util
open Verishot.VisualiseModuleInstance
open Verishot.VisualisePin
open Verishot.VisualiserUtil.Functions
open Verishot.VisualiserUtil.Pin
open Verishot.VisualiseModuleInstanceConnection
open Verishot.VisualisePinConnection
open Verishot.VisualiseConstant
open Verishot.VisualiseStyles

let visualiseNodes (inputPins, moduleInstances, outputPins, _) declMap currDecl =
    let inputPinsIds = inputPins |> List.map fst
    
    let nodeMap0, idx0, _      = visualisePins currDecl inputPinsIds Input Map.empty 0 (0., 0.5)
    let nodeMap1, idx1, coord1 = visualiseModuleInstances moduleInstances declMap nodeMap0 idx0 (defaultPinMargin + defaultPinProps.width, 0.)
    let nodeMap, _, _          = visualisePins currDecl outputPins Output nodeMap1 idx1 (fst coord1, 0.5)
    
    nodeMap

let visualiseConnections (inputPins, moduleInstances, _, _) nodeMap =
    let inputConsSVG, nextId = visualiseInputPinConnections inputPins nodeMap 0
    let moduleConsSVG, _ = visualiseModuleInstanceConnections moduleInstances nodeMap nextId
    [inputConsSVG; moduleConsSVG] |> groupSVG [("class", "connections")] None

let splitNodes (nodes: Node list) =
    let inputPins =
        nodes
        |> List.choose
            (function 
            | InputPin (x, y) -> Some (x, y)
            | _ -> None)
    
    let moduleInstances = 
        nodes 
        |> List.choose
            (function
            | ModuleInstance x -> Some x
            | _ -> None)

    let outputPins = 
        nodes 
        |> List.choose 
            (function
            | OutputPin x -> Some x
            | _ -> None)

    let consts = 
        nodes 
        |> List.choose 
            (function
            | Constant x -> Some x
            | _ -> None)
    
    inputPins, moduleInstances, outputPins, consts

let visualiseNetlist netlist declMap: SVGElement =
    // nodes
    let splittedNodes = splitNodes netlist.nodes
    let _, _, _, constants = splittedNodes
   
    let currDecl = getDeclFromDeclMap declMap netlist.moduleName

    let nodeMap = visualiseNodes splittedNodes declMap currDecl

    let blocksSVG = getSVGFromNodeMap nodeMap [("class", "nodes")]
    let constSVG = visualiseConstants constants nodeMap
    let consSVG = visualiseConnections splittedNodes nodeMap

    [blocksSVG; constSVG; consSVG] |> groupSVG [] None

let visualiseNetlists (netlists: Netlist list) (decls: ModuleDecl list) =
    let declMap = 
        decls
        |> List.map (fun decl -> (decl.name, decl))
        |> Map.ofList

    let styles = Some <| loadStyles unitPx
    let script = None

    let toSvg = fun netlist -> netlist.moduleName, visualiseNetlist netlist declMap
    let toString = fun (modName, svg) -> modName, output svg styles script true
    
    netlists 
    |> List.map (toSvg >> toString)