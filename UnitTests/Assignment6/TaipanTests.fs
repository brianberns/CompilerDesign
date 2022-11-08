namespace CompilerDesign.Assignment6

open Microsoft.VisualStudio.TestTools.UnitTesting

open CompilerDesign.Core
open CompilerDesign.UnitTesting

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
    member _.TypeCheck() =

        let pairs =
            [
                "add1(0)", Ok Type.int
                "add1(true)", Error "Expected: Int, Actual: Bool"
                "3 + 4", Ok Type.int
                "3 + a", Error "Unbound identifier: a"
                "if 1 == 2: 3 + 4 else: 5 + 6", Ok Type.int
                "if 1 >= 2: 3 + 4 else: 5 + 6", Ok Type.int
                "if 1 + 2: 3 + 4 else: 5 + 6", Error "Expected: Bool, Actual: Int"
                "if true == false: true else: !(true)", Ok Type.bool
                "if true >= false: true else: !(true)", Error "Expected: Int, Actual: Bool"
                "(add1(0) : Int)", Ok Type.int
                "def plus(x : Int, y : Int) -> Int: x + y plus(3, 4)", Ok Type.int
                "def plus(x : Int, y : Int) -> Int: x + y plus(true, 4)", Error "Expected: Int, Actual: Bool"
                "def plus(x : Bool, y : Int) -> Int: x + y plus(true, 4)", Error "Expected: Int, Actual: Bool"
                "def plus(x, y): x + y plus(3, 4)", Error "Missing type"
                "let x : Int = 1, y : Int = x + 2 in (x + y : Int)", Ok Type.int
            ]

        for text, expected in pairs do
            let parsed = Parser.parse text
            match parsed with
                | Ok program ->
                    let actual = TypeCheck.typeOf program
                    Assert.AreEqual(expected, actual, text)
                | Error msg ->
                    Assert.Fail($"{text}\n{msg}")

    [<TestMethod>]
    member _.Unify() =
        let tuples =
            [
                    // unify 'X -> Int and Bool -> 'Y under the substitution ['X = Bool, 'Y = Int]
                TypeArrow {
                    InputTypes = [TypeVariable (IdentifierDef.create "X")]
                    OutputType = Type.int
                    Tag = ()
                },
                TypeArrow {
                    InputTypes = [Type.bool]
                    OutputType = TypeVariable (IdentifierDef.create "Y")
                    Tag = ()
                },
                Ok [
                    IdentifierDef.create "X", Type.bool
                    IdentifierDef.create "Y", Type.int
                ]
                    // no substitution that can unify Int -> 'X with Bool -> 'Y
                TypeArrow {
                    InputTypes = [Type.int]
                    OutputType = TypeVariable (IdentifierDef.create "X")
                    Tag = ()
                },
                TypeArrow {
                    InputTypes = [Type.bool]
                    OutputType = TypeVariable (IdentifierDef.create "Y")
                    Tag = ()
                },
                Error "Could not unify"

                    // cannot unify 'A with 'A -> 'B, because we would get the absurd substitution ['A = 'A -> 'B]
                TypeVariable (IdentifierDef.create "A"),
                TypeArrow {
                    InputTypes = [TypeVariable (IdentifierDef.create "A")]
                    OutputType = TypeVariable (IdentifierDef.create "B")
                    Tag = ()
                },
                Error "Could not unify"

                    // can't unify 'A -> 'A with Int -> Bool
                TypeArrow {
                    InputTypes = [TypeVariable (IdentifierDef.create "A")]
                    OutputType = TypeVariable (IdentifierDef.create "A")
                    Tag = ()
                },
                TypeArrow {
                    InputTypes = [Type.int]
                    OutputType = Type.bool
                    Tag = ()
                },
                Error "Could not unify"

                    // unify 'A -> 'A with Int -> 'B
                TypeArrow {
                    InputTypes = [TypeVariable (IdentifierDef.create "A")]
                    OutputType = TypeVariable (IdentifierDef.create "A")
                    Tag = ()
                },
                TypeArrow {
                    InputTypes = [Type.int]
                    OutputType = TypeVariable (IdentifierDef.create "B")
                    Tag = ()
                },
                Ok [
                    IdentifierDef.create "A", Type.int
                    IdentifierDef.create "B", Type.int
                ]
            ]
        for (typ1, typ2, expected) in tuples do
            let actual = Substitution.unify typ1 typ2
            Assert.AreEqual(expected, actual)
