﻿namespace CompilerDesign.Assignment6

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck

module Generator =

    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module IdentifierDef =

    let arb =
        Gen.elements ['a' .. 'z']       // limit to single character names for simplicity
            |> Gen.map (string >> IdentifierDef.create)
            |> Arb.fromGen

module LetDef =

    let arb =
        gen {
            let! bindings =
                Generator.from<NonEmptyArray<_>>   // ensure there is at least one binding
            let! expr =
                Generator.from<Expression<_>>
            return {
                Bindings = Seq.toList bindings.Get
                Expr = expr
                Tag = ()
            }
        } |> Arb.fromGen

module TypeArrowDef =

    let arb =
        gen {
            let! inputs =
                Generator.from<NonEmptyArray<_>>   // ensure there is at least one input type
            let! output =
                Generator.from<Type<_>>
            return {
                InputTypes = Seq.toList inputs.Get
                OutputType = output
                Tag = ()
            }
        } |> Arb.fromGen

module Type =

    let arb =
        gen {
            match! Gen.choose (1, 4) with
                | 1 -> return TypeBlank ()
                | 2 ->
                    return! Gen.elements [   // choose from actual type constants
                        Type.int
                        Type.bool
                    ]
                | 3 ->
                    let! ident = Generator.from<IdentifierDef<_>>
                    return TypeVariable ident
                | 4 ->
                    let! def = Generator.from<TypeArrowDef<_>>
                    return TypeArrow def
                | _ -> return failwith "Unexpected"
        } |> Arb.fromGen

module Decl =

    let arb =
        gen {
            let! ident = Generator.from<IdentifierDef<_>>
            let! parmPairs =
                Generator.from<List<IdentifierDef<_> * Type<_>>>   // ensure # of parameters = # of input types
            let parms, parmTypes = List.unzip parmPairs
            let! tvIdents = Generator.from<List<IdentifierDef<_>>>
            let! outType =
                Generator.from<Type<_>>
            let! body = Generator.from<Expression<_>>

            return {
                Identifier = ident
                Parameters = parms
                Scheme =
                    {
                        TypeVariableIdents = tvIdents
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

module DeclGroup =

    let arb =
        gen {
            let! decls =
                Generator.from<NonEmptyArray<Decl<unit>>>   // ensure at least one decl per group
            return { Decls = Seq.toList decls.Get }
        } |> Arb.fromGen

type Arbitraries =
    static member LetDef() = LetDef.arb
    static member IdentifierDef() = IdentifierDef.arb
    static member TypeArrowDef() = TypeArrowDef.arb
    static member Type() = Type.arb
    static member Decl() = Decl.arb
    static member DeclGroup() = DeclGroup.arb

[<AutoOpen>]
module Untype =

    module Expression =

        let rec untype = function
            | LetExpr def->
                LetExpr {
                    def with
                        Bindings =
                            def.Bindings
                                |> List.map (fun binding ->
                                    {
                                        binding with
                                            Type = TypeBlank ()
                                            Expr = untype binding.Expr
                                    })
                        Expr = untype def.Expr
                }
            | Prim1Expr def ->
                Prim1Expr {
                    def with
                        TypeArguments = []
                        Expr = untype def.Expr
                }
            | Prim2Expr def ->
                Prim2Expr {
                    def with
                        TypeArguments = []
                        Left = untype def.Left
                        Right = untype def.Right
                }
            | IfExpr def ->
                IfExpr {
                    def with
                        Condition = untype def.Condition
                        TrueBranch = untype def.TrueBranch
                        FalseBranch = untype def.FalseBranch
                }
            | ApplicationExpr def ->
                ApplicationExpr {
                    def with
                        Identifier = IdentifierDef.untag def.Identifier
                        TypeArguments = []
                        Arguments =
                            def.Arguments |> List.map untype
                }
            | AnnotationExpr def ->
                untype def.Expr
            | expr -> expr

    module Scheme =

        let untype (scheme : Scheme<_>) =
            match scheme.Type with
                | TypeArrow def ->
                    {
                        scheme with
                            TypeVariableIdents = []
                            Type =
                                TypeArrow {
                                    InputTypes =
                                        def.InputTypes
                                            |> List.map (fun _ -> TypeBlank ())
                                    OutputType = TypeBlank ()
                                    Tag = ()
                                }
                    }
                | _ -> failwith "Unexpected"

    module Decl =

        let untype decl =
            {
                decl with
                    Scheme = Scheme.untype decl.Scheme
                    Body = Expression.untype decl.Body
            }

    module DeclGroup =

        let untype group =
            {
                Decls =
                    List.map Decl.untype group.Decls
            }

    module Program =

        let untype program =
            {
                program with
                    DeclGroups =
                        List.map DeclGroup.untype program.DeclGroups
                    Main =
                        Expression.untype program.Main
            }

[<TestClass>]
type FuzzTests() =

    let config =
        { Config.QuickThrowOnFailure with
            Arbitrary = [ typeof<Arbitraries> ]
            MaxTest = 1000
            Replay = Some (Random.StdGen (0, 0)) }

    let toString (subst : Substitution<_>) =
        subst
            |> Seq.map (fun (ident, typ) ->
                $"'{ident.Name} : {Type.unparse typ}")
            |> String.concat "\n"

    let unify (typ1 : Type<unit>) (typ2 : Type<unit>) =
        match Substitution.unify typ1 typ2 with
            | Ok subst ->
                let nDistinct =
                    subst
                        |> Seq.distinctBy fst
                        |> Seq.length
                if nDistinct = Seq.length subst then
                    let typ1' = Substitution.Type.apply subst typ1
                    let typ2' = Substitution.Type.apply subst typ2
                    let msg =
                        sprintf "\nType 1: %s\nType 2: %s\nSubstitution:\n%s"
                            (Type.unparse typ1)
                            (Type.unparse typ2)
                            (toString subst)
                    typ1' = typ2' |@ msg
                else
                    false |@ toString subst
            | _ -> true |@ ""

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
    member _.UnifyTypes() =
        let config = { config with MaxTest = 10000 }
        Check.One(config, unify)

    [<TestMethod>]
    member _.UnifyTypeArrows() =

        let unifyArrows arrow1 arrow2 =
            unify (TypeArrow arrow1) (TypeArrow arrow2)

        let config = { config with MaxTest = 100000 }
        Check.One(config, unifyArrows)

    [<TestMethod>]
    member _.InferType() =

        let reannotateUntypedIsOriginal (program : Program<unit>) =
            match TypeInfer.annotate program with
                | Ok annotated ->
                    let untyped = Program.untype annotated
                    match TypeInfer.annotate untyped with
                        | Ok reannotated ->
                            if reannotated = annotated then
                                true |@ ""
                            else
                                false |@ Program.unparse program
                        | Error msg -> false |@ msg
                | Error _ -> true |@ ""

        Check.One(config, reannotateUntypedIsOriginal)
