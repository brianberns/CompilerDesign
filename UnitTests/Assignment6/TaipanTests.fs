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
    member _.Annotation() =
        let text =
            """
            def whatever(x):
              let y : Int = x + 5 in # type-annotations on let-bindings do not need parens
              (x : Int) + y # type-annotated variables must be surrounded by parens

            # parameters to function definitions do not need parens
            def plus(x : Int, y : Int) -> Int: x + y

            plus(whatever(2), 3)
            """
        Assert.AreEqual(Ok "12", run text)

    [<TestMethod>]
    member _.BoolMainResult() =
        let text =
            """
            def whatever<'a>(anything : 'a) -> 'a:
              print<'a>(anything)

            (3 ==<Int> print<Int>(whatever<Int>(5)) : Bool)
            """
        Assert.AreEqual(Ok "5\n5\nFalse", run text)

    [<TestMethod>]
    member _.Polymorphic() =
        let text =
            """
            def identity(x): x

            identity(true)
            """
        Assert.AreEqual(Ok "True", run text)

    [<TestMethod>]
    member _.TypeCheckExpr() =

        let pairs =
            [
                "add1(0)", Ok Type.int
                "add1(true)", Error "Expected: Int, Actual: Bool"
                "3 + 4", Ok Type.int
                "3 + a", Error "Untyped expression"
                "if 1 == 2: 3 + 4 else: 5 + 6", Ok Type.int
                "if 1 >= 2: 3 + 4 else: 5 + 6", Ok Type.int
                "if 1 + 2: 3 + 4 else: 5 + 6", Error "Expected: Bool, Actual: Int"
                "if true == false: true else: !(true)", Ok Type.bool
                "if true >= false: true else: !(true)", Error "Expected: Int, Actual: Bool"
            ]

        for text, expected in pairs do
            let parsed = Parser.parse text
            match parsed with
                | Ok program ->
                    let actual = Expr.typeOf () program.Main
                    Assert.AreEqual(expected, actual)
                | Error msgs ->
                    let str =
                        [
                            yield text
                            yield! msgs
                        ] |> String.concat "\n"
                    Assert.Fail(str)
