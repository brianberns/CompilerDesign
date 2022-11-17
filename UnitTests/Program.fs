open CompilerDesign.Assignment6

open CompilerDesign.Core
open CompilerDesign.UnitTesting

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

result {
    let! program = Parser.parse text

    let! program' = TypeInfer.annotate program
    printfn "Inferred:\n%s" <| Program.unparse program'

    printfn "\nCompiled:"
    let assemblyName = "Taipan"
    do! Compiler.compile assemblyName text
    return! Process.run assemblyName
} |> printfn "\n%A"
