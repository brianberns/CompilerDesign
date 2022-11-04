namespace CompilerDesign.Assignment2

open Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core
open CompilerDesign.UnitTesting

[<TestClass>]
type AdderTests() =

    let run text =
        let assemblyName = "Adder"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.``5``() =
        Assert.AreEqual(Ok "5", run "5")

    [<TestMethod>]
    member _.``(sub1 (add1 (sub1 5)))``() =
        Assert.AreEqual(
            Ok "4",
            run "(sub1 (add1 (sub1 5)))")

    [<TestMethod>]
    member _.``(let ((x 5)) (add1 x))``() =
        let text =
            """
            (let ((x 5))
                (add1 x))
            """
        Assert.AreEqual(Ok "6", run text)

    [<TestMethod>]
    member _.``(let ((x 5) (y (sub1 x))) (sub1 y))``() =
        let text =
            """
            (let ((x 5)
                  (y (sub1 x)))
                (sub1 y))
            """
        Assert.AreEqual(Ok "3", run text)

    [<TestMethod>]
    member _.DuplicateBinding() =
        let text =
            """
            (let ((x 5)
                  (x (sub1 x)))
                (sub1 x))
            """
        Assert.AreEqual(
            Error "Variable already exists: x",
            run text)

    [<TestMethod>]
    member _.UnboundIdentifier() =
        let text =
            """
            (let ((x 5))
                (add1 y))
            """
        Assert.AreEqual(
            Error "Unbound identifier: y",
            run text)
