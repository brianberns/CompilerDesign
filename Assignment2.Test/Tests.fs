namespace Assignment2

open Microsoft.VisualStudio.TestTools.UnitTesting
open Assignment1

[<TestClass>]
type AdderTests () =

    [<TestMethod>]
    member _.``5`` () =
        Assert.AreEqual<_>(
            Ok "5",
            Compiler.run "5")

    [<TestMethod>]
    member _.``(sub1 (add1 (sub1 5)))`` () =
        Assert.AreEqual<_>(
            Ok "4",
            Compiler.run "(sub1 (add1 (sub1 5)))")

    [<TestMethod>]
    member _.``(let ((x 5)) (add1 x))`` () =
        let text =
            """
            (let ((x 5))
                (add1 x))
            """
        Assert.AreEqual<_>(
            Ok "6",
            Compiler.run text)
