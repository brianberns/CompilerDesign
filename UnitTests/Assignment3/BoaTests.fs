namespace CompilerDesign.Assignment3

open Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core
open CompilerDesign.UnitTesting

[<TestClass>]
type BoaTests() =

    let run text =
        let assemblyName = "Boa"
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
        Assert.AreEqual(Ok "25", run text)
