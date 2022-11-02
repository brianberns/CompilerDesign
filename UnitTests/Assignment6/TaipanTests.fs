namespace CompilerDesign.Assignment6

open Microsoft.VisualStudio.TestTools.UnitTesting
open CompilerDesign.Core

[<TestClass>]
type TaipanTests() =

    let run text =
        let assemblyName = "Taipan"
        result {
            do! Compiler.compile assemblyName text
            return! Process.run assemblyName
        }

    [<TestMethod>]
    member _.Def() =
        let text =
            """
            def whatever(x):
              let y : Int = x + 5 in # type-annotations on let-bindings do not need parens
              (x : Int) + y # type-annotated variables must be surrounded by parens

            whatever(2)
            """
        Assert.AreEqual<_>(Ok "9", run text)
