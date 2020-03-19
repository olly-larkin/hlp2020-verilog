(*
    Front end code for `Project` managing
    =====================================
    Author: lhl2617
*)


module Verishot.Project

open System.IO
open System.Text.RegularExpressions

open Verishot.FrontEnd
open Verishot.Util
open Verishot.FileUtil
open Verishot.FrontEndParser

let getNumberOfVProjFiles workspacePath = 
    workspacePath
    |> getFilenamesInFolderPath "*.vproj"
    |> List.length

let getModuleTemplate moduleName = 
    sprintf "module %s (\n);\nendmodule" moduleName

let createNewModule vProjFilePath moduleName = 
    let exitCode = exitCodes.NewModuleError
    match matchIdentifierRegex moduleName with
    | true -> 
        let currentModules = getExistingModuleNamesFromVProj vProjFilePath
        match List.exists ((=) moduleName) currentModules with 
        | false ->
            // add to .vproj
            let vProjContent = readFileToString vProjFilePath + sprintf "\n%s.v" moduleName
            writeStringToFile vProjFilePath vProjContent

            // create the new module
            let workspacePath = Directory.GetParent(vProjFilePath).FullName
            let moduleTemplateContent = getModuleTemplate moduleName
            let moduleFilepath = workspacePath +/ moduleName + ".v"
            writeStringToFile moduleFilepath moduleTemplateContent

            let stdout = sprintf "Successfully created module `%s`" moduleName
            Ok stdout
        | _ -> 
            let stdout = "Module name already exists"
            Error (exitCode, stdout)
    | _ -> 
        let stdout = "Enter a valid module name"
        Error (exitCode, stdout)

let deleteModule vProjFilePath moduleName = 
    let exitCode = exitCodes.DeleteModuleError
    let currentModules = getExistingModuleNamesFromVProj vProjFilePath
    match List.exists ((=) moduleName) currentModules with 
    | true ->
        // update .vproj
        let vProjContent =
            vProjFilePath
            |> readFileToStringList
            |> List.filter (fun x -> x.Trim() |> (<>) (moduleName + ".v"))
            |> String.concat "\n"
        writeStringToFile vProjFilePath vProjContent

        // delete .v
        let workspacePath = Directory.GetParent(vProjFilePath).FullName
        let vFilePath = workspacePath +/ moduleName + ".v"
        deleteFile vFilePath

        let stdout = sprintf "Successfully deleted module `%s`" moduleName
        Ok stdout
    | _ -> 
        let stdout = sprintf "Module `%s` does not exist" moduleName
        Error (exitCode, stdout)


let createNewProject workspacePath projectName = 
    let exitCode = exitCodes.NewProjectError
    
    let vProjHeader = "// ===== Verishot Project File =====
// WARNING: Do not edit this file directly unless you know what you are doing!
// 
// The first module listed must be the top level module"

    match getNumberOfVProjFiles workspacePath with 
    | 0 -> 
        match matchIdentifierRegex projectName with
        | true -> 
            let vProjFilePath = workspacePath +/ projectName + ".vproj"
            writeStringToFile vProjFilePath vProjHeader

            match createNewModule vProjFilePath projectName with
            | Ok stdout ->
                let stdout2 = sprintf "Successfully created project `%s`" projectName
                let stdout' = stdout +@ stdout2
                Ok stdout'
            | Error x -> Error x           
        | _ ->
            let stdout = sprintf "Enter a valid project name"
            Error (exitCode, stdout)
    | _ -> 
        let stdout = sprintf "This workspace already contains an existing project"
        Error (exitCode, stdout)

let listModules vProjFilePath = 
    getExistingModuleNamesFromVProj vProjFilePath 
    |> String.concat "\n" 
    |> Ok