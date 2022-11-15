open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def f(x): x + 6
    f(38)
    """

let program =
    text
        |> Parser.parse
        |> Result.get

result {

    let! program' = TypeInfer.annotate program
    printfn "Inferred:\n%s" <| Program.unparse program'

    let! typ = TypeCheck.typeOf program'
    printfn "\nChecked: %s" (Type.unparse typ)

    do! Compiler.compile "Taipan" text

} |> printfn "\n%A"
