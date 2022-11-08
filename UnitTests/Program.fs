open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    "add1(print(0))"
        |> Parser.parse
        |> Result.get
TypeInfer.inferType program.Main
    |> Result.map Type.unparse
    |> printfn "%A"
