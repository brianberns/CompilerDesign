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
                "def f(x : Int, y : Int) -> Int: x + y f(0)", Error "Arity mismatch: expected 2, actual 1"
                "f(0)", Error "Unbound identifier: f"
                "let x : Int = 0, x : Bool = true in x", Error "Duplicate identifier: x"
                "def f(x : Int, x : Int) -> Int: x 0", Error "Duplicate identifier: x"

                """
                def f(x : Int) -> Int: 0
                def f(x : Int) -> Int: 1
                f(0)
                """, Error "Duplicate identifier: f"

                """
                def a() -> Int: b()
                and def b() -> Int: a()
                a()
                """, Ok Type.int

                """
                def id<'t>(x : 't) -> 't: x
                id(0)
                """, Ok Type.int
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

        let parseType text =
            Parser.Scheme.parse text
                |> Result.get
                |> (fun scheme ->
                    Assert.IsTrue(scheme.TypeVariableIdents.IsEmpty)
                    scheme.Type)

        let tuples =
            [
                    // unify 'X -> Int and Bool -> 'Y under the substitution ['X = Bool, 'Y = Int]
                "('X -> Int)",
                "(Bool -> 'Y)",
                Ok [
                    IdentifierDef.create "X", Type.bool
                    IdentifierDef.create "Y", Type.int
                ]
                    // no substitution that can unify Int -> 'X with Bool -> 'Y
                "(Int -> 'X)",
                "(Bool -> 'Y)",
                Error "Could not unify Int and Bool"

                    // cannot unify 'A with 'A -> 'B, because we would get the absurd substitution ['A = 'A -> 'B]
                "'A",
                "('A -> 'B)",
                Error "Could not unify 'A and ('A -> 'B)"

                "('A -> 'A)",
                "(Int -> Bool)",
                Error "Could not unify Int and Bool"

                "('A -> 'A)",
                "(Int -> 'B)",
                Ok [
                    IdentifierDef.create "A", Type.int
                    IdentifierDef.create "B", Type.int
                ]
            ]
        for (text1, text2, expected) in tuples do
            let actual =
                let typ1 = parseType text1
                let typ2 = parseType text2
                Substitution.unify typ1 typ2
            Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member _.TypeInfer() =

        let pairs =
            [
                "let x = 1, y = x + 2 in x + y", Ok Type.int
                "let x = 0, x = true in x", Error "Duplicate identifier: x"
                "add1(false)", Error "Could not unify Int and Bool"
                "(0 : 'a)", Ok Type.int
                "(0 : _)", Ok Type.int
                "(false : Int)", Ok Type.bool
            ]

        for text, expected in pairs do
            let parsed = Parser.parse text
            match parsed with
                | Ok program ->
                    let actual = TypeInfer.typeOf program
                    Assert.AreEqual(expected, actual, text)
                | Error msg ->
                    Assert.Fail($"{text}\n{msg}")

    [<TestMethod>]
    member _.Polymorphic() =
        let text =
            """
            def identity(x): x

            identity(true)
            """
        Assert.AreEqual(Ok "True", run text)
