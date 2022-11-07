namespace CompilerDesign.Assignment6

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module IdentifierDef =

    let arb =
        Gen.elements ['a' .. 'z']
            |> Gen.map (fun c ->
                {
                    Name = string c
                    Tag = ()
                })
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

module TypeArrowDef =

    let arb =
        gen {
            let! inputs =
                Generator.from<NonEmptyArray<_>>
            let! output =
                Generator.from<Type<_>>
            return {
                InputTypes = Seq.toList inputs.Get
                OutputType = output
                Tag = ()
            }
        } |> Arb.fromGen

module Decl =

    let arb =
        gen {
            let! ident = Generator.from<IdentifierDef<_>>
            let! tvIdents = Generator.from<List<IdentifierDef<_>>>
            let! parmPairs =
                Generator.from<List<IdentifierDef<_> * Type<_>>>
            let! outType =
                Generator.from<Type<_>>

            let! body = Generator.from<Expr<_>>

            let parms, parmTypes = List.unzip parmPairs

            return {
                Identifier = ident
                Parameters = parms
                Scheme =
                    {
                        Identifiers = tvIdents
                        Type =
                            TypeArrow {
                                InputTypes = parmTypes
                                OutputType = outType
                                Tag = ()
                            }
                        Tag = ()
                    }
                Body = body
            }
        } |> Arb.fromGen

type Arbitraries =
    static member LetDef() = LetDef.arb
    static member NumberDef() = NumberDef.arb
    static member IdentifierDef() = IdentifierDef.arb
    static member TypeArrowDef() = TypeArrowDef.arb
    static member Decl() = Decl.arb

[<TestClass>]
type FuzzTests() =

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary = [ typeof<Arbitraries> ]
            MaxTest = 1000
            Replay = Some (Random.StdGen (0, 0)) }

    [<TestMethod>]
    member _.ParseUnparseIsOriginal() =

        let parseUnparseIsOriginal program =
            let unparsed = Program.unparse program
            let reparsed =
                Parser.parse unparsed
                    |> Result.map Program.untag
            let msg = sprintf "Text: %s\nResult: %A" unparsed reparsed
            reparsed = Ok program |@ msg

        Check.One(config, parseUnparseIsOriginal)

    [<TestMethod>]
    member _.Unify() =

        let apply (typ1 : Type<unit>) (typ2 : Type<unit>) =
            match TypeInfer.unify typ1 typ2 with
                | Ok subst ->
                    let typ1' = TypeInfer.Type.apply subst typ1
                    let typ2' = TypeInfer.Type.apply subst typ2
                    typ1' = typ2' |@ sprintf "\n%A" subst
                | _ -> true |@ ""

        Check.One(config, apply)
