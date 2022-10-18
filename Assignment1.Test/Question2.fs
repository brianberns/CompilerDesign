namespace Assignment1

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Question2() =

    [<TestMethod>]
    member _.``3 * (4 + 5)``() =
        let arith = Times(Num 3, Plus(Num 4, Num 5))
        Assert.AreEqual("(3 * (4 + 5))", Arith.pretty arith Env.empty)
        Assert.AreEqual(27, Arith.evaluate arith Env.empty)

    [<TestMethod>]
    member _.``(3 * 4) + 5``() =
        let arith = Plus(Times(Num 3, Num 4), Num 5)
        Assert.AreEqual("((3 * 4) + 5)", Arith.pretty arith Env.empty)
        Assert.AreEqual(17, Arith.evaluate arith Env.empty)

    [<TestMethod>]
    member _.``x + 1``() =
        let env =
            Env.add Env.empty "x" 2
        let arith =
            Plus(Variable "x", Num 1)
        Assert.AreEqual("(x + 1)", Arith.pretty arith env)
        Assert.AreEqual(3, Arith.evaluate arith env)

    [<TestMethod>]
    member _.``Invalid``() =
        let env = Env.empty
        let arith =
            Plus(Variable "x", Num 1)
        Assert.AreEqual("(x + 1)", Arith.pretty arith env)
        Assert.ThrowsException(fun () ->
            Arith.evaluate arith env :> obj)
            |> ignore
