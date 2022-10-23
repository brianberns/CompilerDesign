namespace CompilerDesign.Assignment3

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type BoaTests() =

    [<TestMethod>]
    member _.Parse() =
        let text = "let a = b, c = d in a + c"
        let actual = Expr.parse text
        printfn "%A" actual
