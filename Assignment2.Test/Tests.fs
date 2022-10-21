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
