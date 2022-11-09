open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    "let x = false in print(x)"
        |> Parser.parse
        |> Result.get
TypeInfer.inferType program.Main
    |> Result.map Type.unparse
    |> printfn "%A"
