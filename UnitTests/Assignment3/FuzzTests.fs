namespace CompilerDesign.Assignment3

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module NumberDef =

    let arb =
        Generator.from<int>
            |> Gen.map (fun n ->
                {
                    Number = n
                    Tag = ()
                })
            |> Arb.fromGen

module IdentifierDef =

    let arb =
        Generator.from<NonWhiteSpaceString>
            |> Gen.where (fun nwss ->
                nwss.Get |> Seq.forall Char.IsLetter)
            |> Gen.map (fun nwss ->
                {
                    Identifier = nwss.Get
                    Tag = ()
                })
            |> Arb.fromGen

type Arbitraries =
    static member NumberDef() = NumberDef.arb
    static member IdentifierDef() = IdentifierDef.arb

[<TestClass>]
type FuzzTests() =

    let rec untag = function
        | LetExpr def ->
            LetExpr {
                Bindings =
                    def.Bindings
                        |> List.map (fun binding ->
                            {
                                Identifier = binding.Identifier
                                Expr = untag binding.Expr
                                Tag = ()
                            })
                Expr = untag def.Expr
                Tag = ()
            }
        | Prim1Expr def ->
            Prim1Expr {
                Operator = def.Operator
                Expr = untag def.Expr
                Tag = ()
            }
        | Prim2Expr def ->
            Prim2Expr {
                Operator = def.Operator
                Left = untag def.Left
                Right = untag def.Right
                Tag = ()
            }
        | IfExpr def ->
            IfExpr {
                Condition = untag def.Condition
                TrueBranch = untag def.TrueBranch
                FalseBranch = untag def.FalseBranch
                Tag = ()
            }
        | NumberExpr def ->
            NumberExpr {
                Number = def.Number
                Tag = ()
            }
        | IdentifierExpr def ->
            IdentifierExpr {
                Identifier = def.Identifier
                Tag = ()
            }

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal expr =
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
