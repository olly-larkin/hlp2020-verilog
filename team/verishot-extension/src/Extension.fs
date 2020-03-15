module VerishotExtension.Extension

open VerishotExtension.Util
open VerishotExtension.Verishot

open Fable.Import
open Fable.Import.JS
open Fable.Import.Browser
open Fable.Import.vscode
open Fable.Core.JsInterop


let activate (context: vscode.ExtensionContext) =  
    printfn "Verishot Extension acivated"
    // vscode.commands.registerCommand("extension.helloWorld", fun _ -> 
    //     vscode.window.showInformationMessage "Hello!" |> unbox )
    // |> context.subscriptions.Add
    
    // Lint, Simulate, Visualise
    vscode.commands.registerCommand("extension.lint", fun _ ->
        execVerishot Lint)
    |> context.subscriptions.Add

    vscode.commands.registerCommand("extension.simulate", fun _ ->
        execVerishot Simulate)
    |> context.subscriptions.Add

    vscode.commands.registerCommand("extension.visualise", fun _ ->
        execVerishot Visualise)
    |> context.subscriptions.Add

