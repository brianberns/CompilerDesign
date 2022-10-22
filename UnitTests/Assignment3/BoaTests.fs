namespace CompilerDesign.Assignment3

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type BoaTests() =

    [<TestMethod>]
    member _.Parse() =
        let text = "add1(x) + sub1(2) * add1(3)"
        let actual = Expr.parse text
        printfn "%A" actual
