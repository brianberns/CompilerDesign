open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    let x = 1 in x + x
    """
        |> Parser.parse
        |> Result.get

result {
    let! program' = TypeInfer.annotate program
    printfn "%s" <| Program.unparse program'
    return! TypeCheck.typeOf program'
} |> printfn "%A"
