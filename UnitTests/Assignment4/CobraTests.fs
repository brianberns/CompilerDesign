namespace CompilerDesign.Assignment4

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.Core

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
                    if a : asq + bsq
                    else: add1(a)
            """
        Assert.AreEqual<_>(Ok "25", run text)

    [<TestMethod>]
    member _.WontRun() =
        let text =
            """
            if true > 0 : print(100) else: print(false)
            """
        match run text with
            | Error [| msg |] ->
                Assert.IsTrue(
                    msg.Contains(
                        "Operator '>' cannot be applied to operands of type 'bool' and 'int'"))
            | _ -> Assert.Fail()
