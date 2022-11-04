namespace CompilerDesign.Assignment1

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.UnitTesting

/// Question 6.
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
        let actual = SExp.parse "(a b)"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member _.``(a (b true) 3)``() =
        let expected =
            Ok [
                Nest (
                    [
                        Sym ("a", (0, 1, 0, 2))
                        Nest (
                            [
                                Sym ("b", (0, 4, 0, 5))
                                Bool (true, (0, 6, 0, 10))
                            ],
                            (0, 3, 0, 11))
                        Int (3, (0, 12, 0, 13))
                    ],
                    (0, 0, 0, 14))
            ]
        let actual = SExp.parse "(a (b true) 3)"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member _.``(a``() =
        let expected = Error "Unmatched left paren at line 0, col 0"
        let actual = SExp.parse "(a"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member _.``(a (b c``() =
        let expected = Error "Unmatched left paren at line 0, col 3"
        let actual = SExp.parse "(a (b c"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member _.``()``() =
        let expected =
            Ok [
                Nest (
                    List.empty,
                    (0, 0, 0, 2))
            ]
        let actual = SExp.parse "()"
        Assert.AreEqual(expected, actual)
