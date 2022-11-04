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
