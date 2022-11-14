open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def f(x): # should have scheme Forall 'X, ('X -> 'X)
        print(x)

    def ab_bool(a, b): # should have scheme Forall 'A, ('A, Bool -> Bool)
        isnum(f(a)) && f(b)

    ab_bool(3, true) && ab_bool(true, false)
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
