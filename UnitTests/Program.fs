open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    def whatever(x : Int):
        (x : Int)

    whatever(2)
    """
        |> Parser.parse
        |> Result.get

result {
    let! program' = TypeInfer.annotate program
    printfn "Inferred:\n%s" <| Program.unparse program'
    let! typ = TypeCheck.typeOf program'
    printfn "\nChecked: %s" (Type.unparse typ)
} |> printfn "\n%A"
