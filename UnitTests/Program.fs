open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
        def even(n):
            !(odd(n))

        and def odd(n):
            if n == 0: false
            else: if n == 1: true
            else:
            even(n - 1)

        odd(5)
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
