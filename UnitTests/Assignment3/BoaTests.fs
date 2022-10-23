namespace CompilerDesign.Assignment3

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type BoaTests() =

    [<TestMethod>]
    member _.Parse() =
        let text = "if 1 : 2 else: 3"
        let actual = Expr.parse text
        printfn "%A" actual
