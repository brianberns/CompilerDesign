open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    "0 == false"
        |> Parser.parse
        |> Result.get
TypeInfer.inferType program.Main
    |> Result.map Type.unparse
    |> printfn "%A"
