module LongArithm.Cli.Main

open Argu
open LongArithm.Interpreter
    
type CLIArguments =
    | InputFile of file: string
    | InputString of code: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputFile _ -> "File with code"
            | InputString _ -> "String of code" 

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Long Arithm Interpreter")
    let results = parser.Parse(argv)
    let p = parser.ParseCommandLine argv
    if argv.Length = 0 || results.IsUsageRequested then parser.PrintUsage() |> printfn "%s"
    else
        let input =
            if p.Contains(InputFile) then System.IO.File.ReadAllText (results.GetResult InputFile) 
            elif p.Contains(InputString) then results.GetResult InputString
            else invalidArg "argv" "No input file or string given"
        match Runners.runTryToCatchErrors input with
        | Ok state -> printfn $"State: {state}"
        | Error msg -> printfn $"Error: {msg}"
    0
