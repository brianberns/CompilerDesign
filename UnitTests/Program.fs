open CompilerDesign.Assignment6

open CompilerDesign.Core

let program =
    """
    def f<'a>(x : 'a) -> 'a:
      print(x)

    and def ab_bool<'a, 'b>(a : 'a, b : 'b) -> Bool:
      isnum(f(a)) && f(b)

    ab_bool(3, true) && ab_bool(true, false)
    """
        |> Parser.parse
        |> Result.get

Program.unparse program
    |> printfn "%s"
TypeCheck.typeOf program
    |> printfn "%A"
