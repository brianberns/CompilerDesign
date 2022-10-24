namespace CompilerDesign.Assignment3

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module Number =

    let arb =
        Generator.from<int>
            |> Gen.map (fun n ->
                NumberExpr {|
                    Number = n
                    Tag = ()
                |})
            |> Arb.fromGen

module Identifier =

    let arb =
        Generator.from<NonWhiteSpaceString>
            |> Gen.where (fun nwss ->
                nwss.Get |> Seq.forall Char.IsLetter)
            |> Gen.map (fun nwss ->
                IdentifierExpr {|
                    Identifier = nwss.Get
                    Tag = ()
                |})
            |> Arb.fromGen

type Arbitraries =
    static member Number() = Number.arb
    static member Identifier() = Identifier.arb

[<TestClass>]
type FuzzTests() =

    let rec untag = function
        | LetExpr rcd ->
            LetExpr {|
                Bindings =
                    rcd.Bindings
                        |> List.map (fun binding ->
                            {
                                Identifier = binding.Identifier   // why is this necessary?
                                Expr = untag binding.Expr
                                Tag = ()
                            })
                Expr = untag rcd.Expr
                Tag = ()
            |}
        | Prim1Expr rcd ->
            Prim1Expr {|
                rcd with
                    Expr = untag rcd.Expr
                    Tag = ()
            |}
        | Prim2Expr rcd ->
            Prim2Expr {|
                rcd with
                    Left = untag rcd.Left
                    Right = untag rcd.Right
                    Tag = ()
            |}
        | IfExpr rcd ->
            IfExpr {|
                Condition = untag rcd.Condition
                TrueBranch = untag rcd.TrueBranch
                FalseBranch = untag rcd.FalseBranch
                Tag = ()
            |}
        | NumberExpr rcd ->
            NumberExpr {|
                rcd with Tag = ()
            |}
        | IdentifierExpr rcd ->
            IdentifierExpr {|
                rcd with Tag = ()
            |}

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal (expr : Expr<unit>) =
            let unparsed = Expr.unparse expr
            let reparsed =
                Parser.parse unparsed
                    |> Result.map untag
            reparsed = Ok expr |@ unparsed

        let config =
            { Config.QuickThrowOnFailure with
                Arbitrary = [ typeof<Arbitraries> ]
                MaxTest = 1000
                Replay = Some (Random.StdGen (0, 0)) }
        Check.One(config, parseUnparseIsOriginal)
