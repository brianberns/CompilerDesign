open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
def f(x): # should have scheme Forall 'X, ('X -> 'X)
  x

def ab_bool(a, b): # should have scheme Forall 'A, 'B, ('A, 'B -> Bool)
  isnum(f(a)) && f(b)

ab_bool(3, true) && ab_bool(true, false)
    """
        |> Parser.parse
        |> Result.get

result {
    let! typ = TypeInfer.typeOf program
    return Type.unparse typ
} |> printfn "%A"
