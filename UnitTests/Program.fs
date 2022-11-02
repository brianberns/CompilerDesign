open CompilerDesign.Assignment6

let text = "isbool<y>(0)"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
