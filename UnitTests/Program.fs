open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def f(x, y):
        isnum(print(x)) && isbool(y)

    def g(z):
        f(z, 5)

    g(7)
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
