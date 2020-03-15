module VerishotExtension.Verishot

open VerishotExtension.Util

let execVerishot (mode: VerishotMode) = 
    match mode with 
    | Lint -> failwith "TODO"
    | Simulate -> failwith "TODO"
    | Visualise -> failwith "TODO"