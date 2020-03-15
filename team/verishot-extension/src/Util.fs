module VerishotExtension.Util



type VerishotMode = 
    | Lint
    | Simulate
    | Visualise

let readLinesFromFile (filePath: string): string list =
    match fs.existsSync filePath with
    | true -> 
        fs.readFileSync filePath 
    failwith "TOD"