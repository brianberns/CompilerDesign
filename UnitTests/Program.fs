open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    def f(x) : x
    0
    """
        |> Parser.parse
        |> Result.get

result {
    let! program' = TypeInfer.annotate program
    printfn "%s" <| Program.unparse program'
    return! TypeCheck.typeOf program'
} |> printfn "%A"
