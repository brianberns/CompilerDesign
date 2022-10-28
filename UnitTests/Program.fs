open CompilerDesign.Assignment5

let text = "def f(): 0 (-1)"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
