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
    member _.Annotation1() =
        let text =
            """
            def whatever(x):
              let y : Int = x + 5 in # type-annotations on let-bindings do not need parens
              (x : Int) + y # type-annotated variables must be surrounded by parens

            # parameters to function definitions do not need parens
            def plus(x : Int, y : Int) -> Int: x + y

            plus(whatever(2), 3)
            """
        Assert.AreEqual<_>(Ok "12", run text)

    [<TestMethod>]
    member _.Annotation2() =
        let text =
            """
            def whatever<'a>(anything : 'a) -> 'a:
              print<'a>(anything)

            (3 ==<Int> print<Int>(whatever<Int>(5)) : Bool)
            """
        Assert.AreEqual<_>(Ok "5\n5\nFalse", run text)
