namespace CompilerDesign.Assignment4

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

type Identifier =
    Ident of string
    with
    member this.Get =
        let (Ident str) = this
        str

module Identifier =

    let arb =
        Gen.elements ['a' .. 'z']
            |> Gen.map string
            |> Arb.fromGen

module LetDef =

    let arb =
        gen {
            let! bindings =
                Generator.from<NonEmptyArray<_>>
            let! expr =
                Generator.from<Expr<_>>
            return {
                Bindings = Seq.toList bindings.Get
                Expr = expr
                Tag = ()
            }
        } |> Arb.fromGen

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
        Generator.from<Identifier>
            |> Gen.map (fun ident ->
                {
                    Identifier = ident.Get
                    Tag = ()
                })
            |> Arb.fromGen

type Arbitraries =
    static member LetDef() = LetDef.arb
    static member NumberDef() = NumberDef.arb
    static member IdentifierDef() = IdentifierDef.arb
    static member Identifier() = Identifier.arb

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
        | BoolExpr def ->
            BoolExpr {
                Flag = def.Flag
                Tag = ()
            }

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary = [ typeof<Arbitraries> ]
            MaxTest = 1000
            Replay = Some (Random.StdGen (0, 0)) }

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal expr =
            let unparsed = Expr.unparse expr
            let reparsed =
                Parser.parse unparsed
                    |> Result.map untag
            reparsed = Ok expr |@ unparsed

        Check.One(config, parseUnparseIsOriginal)
