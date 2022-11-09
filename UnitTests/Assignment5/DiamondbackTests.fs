namespace CompilerDesign.Assignment5

open Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core
open CompilerDesign.UnitTesting

[<TestClass>]
type DiamondbackTests() =

    let run text =
        let assemblyName = "Diamondback"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.Comment() =
        let text = "1 # comment"
        Assert.AreEqual(Ok "1", run text)

    [<TestMethod>]
    member _.Factorial() =
        let text =
            """
            # recursive factorial function
            def factorial(n):
                if n <= 0: 1
                else: n * factorial(n-1)

            factorial(6)
            """
        Assert.AreEqual(Ok "720", run text)

    [<TestMethod>]
    member _.Arity() =
        let text =
            """
            def f(x, y): x + y
            f(0)
            """
        Assert.AreEqual(
            Error "Arity mismatch: expected 2, actual 1",
            run text)

    [<TestMethod>]
    member _.UnboundFun() =
        let text = "f(0)"
        Assert.AreEqual(
            Error "Function not found: f",
            run text)

    [<TestMethod>]
    member _.UnboundId() =
        let text = "a"
        Assert.AreEqual(
            Error "Unbound identifier: a",
            run text)

    [<TestMethod>]
    member _.DuplicateIdLet() =
        let text = "let x=0, x=true in x"
        Assert.AreEqual(
            Error "Variable already exists: x",
            run text)

    [<TestMethod>]
    member _.DuplicateIdDecl() =
        let text = "def f(x, x): x 0"
        Assert.AreEqual(
            Error "Variable already exists: x",
            run text)

    [<TestMethod>]
    member _.DuplicateFun() =
        let text =
            """
            def f(x): 0
            def f(x): 1
            f(0)
            """
        Assert.AreEqual(
            Error "Function already exists: f",
            run text)
