open CompilerDesign.Assignment6

open CompilerDesign.Core

let text =
    """
    def f(x, y): 0
    0
    """

let program =
    text
        |> Parser.parse
        |> Result.get
        |> printfn "%A"
