open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    "print(0)"
        |> Parser.parse
        |> Result.get
TypeInfer.inferType program.Main
    |> Result.map Type.unparse
    |> printfn "%A"
