open CompilerDesign.Assignment5

let text = "y(1)"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok expr ->
        printfn "%A" (Expr.unparse expr)
    | _ -> ()

