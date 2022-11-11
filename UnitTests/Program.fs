open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    def f(x) : x
    let a = f(0), b = f(false) in 0
    """
        |> Parser.parse
        |> Result.get

result {
    let! program' = TypeInfer.annotate program
    printfn "Inferred:\n%s" <| Program.unparse program'
    let! typ = TypeCheck.typeOf program'
    printfn "\nChecked: %s" (Type.unparse typ)
} |> printfn "\n%A"
