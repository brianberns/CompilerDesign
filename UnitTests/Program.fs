open CompilerDesign.Assignment6

let text = "def j(): z k"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
