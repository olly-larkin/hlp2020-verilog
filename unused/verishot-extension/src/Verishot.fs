module VerishotExtension.Verishot

open VerishotExtension.Util
open Fable.Import
open Fable.Core

type spawnSyncObject = {
    stdout: obj;
    stderr: obj;
    status: obj
}

[<Import("*", "child_process")>]
let cp: Node.ChildProcess.IExports = jsNative

let execVerishot (mode: VerishotMode) = 
    match mode with 
    | Lint -> failwith "TODO"
    | Simulate -> failwith "TODO"
    | Visualise -> failwith "TODO"

let execCmd (statusBarMsg: string) (args: string list) =
    let execution =
        let s = cp.spawnSync ("verishot", ResizeArray<string> args, None)
        let stderrText: string = s.stderr.toString().trim()
        let stdoutText: string = s.stdout.toString().trim()
        if stderrText.Length <> 0
            then vscode.window.showErrorMessage stderrText |> ignore
            else
                let outfunc = if s.status = 0 then vscode.window.showInformationMessage else vscode.window.showErrorMessage
                outfunc stdoutText |> ignore
    let p = 
        promise {
            do! execution
            return 0
        }
    vscode.window.setStatusBarMessage statusBarMsg 
        
    failwith "todo"