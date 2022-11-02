open CompilerDesign.Assignment5

let text = "(1 : int)"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
