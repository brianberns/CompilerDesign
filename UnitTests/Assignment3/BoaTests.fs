namespace CompilerDesign.Assignment3

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.Core

[<TestClass>]
type BoaTests() =

    let run text =
        let assemblyName = "Boa"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.``5``() =
        Assert.AreEqual<_>(Ok "5", run "5")
