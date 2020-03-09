(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Main
    Description: Electron Renderer Process - Script to executed after `index.html` is loaded.
*)

module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node.Exports
open Fable.PowerPack

open Fable.Import.Browser

// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from the renderer.js" |> ignore

open Ref
open Update
open Emulator

/// Access to `Emulator` project
let dummyVariable = Emulator.Common.A


/// Initialization after `index.html` is loaded
let init () =

    // TODO: Implement actions for the buttons
    Ref.explore.addEventListener_click(fun _ ->
        Browser.console.log "Code updated"
        Update.code("mov r7, #5")
    )
    Ref.save.addEventListener_click(fun _ ->
        Browser.window.alert (sprintf "%A" (Ref.code ()))
    )
    Ref.run.addEventListener_click(fun _ ->
        Browser.window.alert "NotImplemented :|"
    )
    Ref.visualise.addEventListener_click(fun _ -> 
        Browser.window.alert "Visualise!"
    )
    Ref.fontSizeChanger.addEventListener_change(fun _ ->
        let size: int =
            // TODO: error-prone, hardcoded index
            // of word "Font Size: xx" to slice
            Ref.fontSizeChanger.value.[11..]
            |> int
        Browser.console.log "Font size updated" |> ignore
        Update.fontSize size
    )

init()