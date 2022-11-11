open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    def id(x) : x
    id(0)
    """
        |> Parser.parse
        |> Result.get

result {
    let! typ = TypeInfer.typeOf program
    return Type.unparse typ
} |> printfn "%A"
