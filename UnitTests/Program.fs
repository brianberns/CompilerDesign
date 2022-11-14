open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def whatever(x):
        let y : Int = x + 5 in # type-annotations on let-bindings do not need parens
        (x : Int) + y # type-annotated variables must be surrounded by parens

    # parameters to function definitions do not need parens
    def plus(x : Int, y : Int) -> Int: x + y

    plus(whatever(2), 3)
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
