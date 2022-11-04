namespace CompilerDesign.Assignment4

open Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core
open CompilerDesign.UnitTesting

[<TestClass>]
type CobraTests() =

    let run text =
        let assemblyName = "Cobra"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.SumOfSquares() =
        let text =
            """
            let a = 3, b = 4 in
                let asq = a * a, bsq = b * b in
                    if a < b : asq + bsq
                    else: add1(a)
            """
        Assert.AreEqual(Ok "25", run text)

    [<TestMethod>]
    member _.Print() =
        let text =
            """
            let x = 1 in
            let y = print(x + 1) in
            print(y + 2)
            """
        Assert.AreEqual(Ok "2\n4\n4", run text)

    [<TestMethod>]
    member _.IfPrint() =
        let text =
            """
            if true : print(100) else: print(99)
            """
        Assert.AreEqual(Ok "100\n100", run text)

    [<TestMethod>]
    member _.WontRun() =
        let text =
            """
            if true > 0 : print(100) else: print(false)
            """
        match run text with
            | Error msg ->
                Assert.IsTrue(
                    msg.Contains(
                        "Operator '>' cannot be applied to operands of type 'bool' and 'int'"))
            | _ -> Assert.Fail()
