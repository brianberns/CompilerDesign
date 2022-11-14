open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def whatever<'a>(anything : 'a) -> 'a:
        print<'a>(anything)

    (3 ==<Int> print<Int>(whatever<Int>(5)) : Bool)
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
