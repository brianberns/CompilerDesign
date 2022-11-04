namespace CompilerDesign.Assignment1

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.UnitTesting

[<TestClass>]
type TokTests() =

    [<TestMethod>]
    member _.``(a b)``() =
        let expected =
            [
                LPAREN (0, 0, 0, 1)
                TSym ("a", (0, 1, 0, 2))
                TSym ("b", (0, 3, 0, 4))
                RPAREN (0, 4, 0, 5)
            ]
        let actual = Tok.tokenize "(a b)"
        Assert.AreEqual(expected, actual)
