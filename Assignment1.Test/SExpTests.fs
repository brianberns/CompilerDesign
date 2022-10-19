namespace Assignment1

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SExpTests() =

    [<TestMethod>]
    member _.``(a b)``() =
        let expected =
            Ok [
                Nest (
                    [
                        Sym ("a", (0, 1, 0, 2))
                        Sym ("b", (0, 3, 0, 4))
                    ],
                    (0, 0, 0, 5))
            ]
        let actual = SExp.parse_toks (Tok.tokenize "(a b)")
        Assert.AreEqual<_>(expected, actual)
