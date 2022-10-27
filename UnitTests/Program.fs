open CompilerDesign.Assignment5

let text = "y()"
let parsed = Parser.parse text
printfn "%A" parsed
match parsed with
    | Ok expr ->
        printfn "%A" (Expr.unparse expr)
    | _ -> ()

