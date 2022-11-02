﻿namespace CompilerDesign.Assignment6

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

type Arbitraries =
    static member LetDef() = LetDef.arb
    static member NumberDef() = NumberDef.arb
    static member IdentifierDef() = IdentifierDef.arb
    static member TypeArrowDef() = TypeArrowDef.arb

[<TestClass>]
type FuzzTests() =

    let untagIdent (ident : IdentifierDef<_>) =
        {
            Name = ident.Name
            Tag = ()
        }

    let rec untagType = function
        | TypeBlank _ -> TypeBlank ()
        | TypeConstant def -> TypeConstant (untagIdent def)
        | TypeVariable def -> TypeVariable (untagIdent def)
        | TypeArrow def ->
            TypeArrow {
                InputTypes =
                    def.InputTypes |> List.map untagType
                OutputType = untagType def.OutputType
                Tag = ()
            }

    let rec untagExpr = function
        | LetExpr def->
            LetExpr {
                Bindings =
                    def.Bindings
                        |> List.map (fun binding ->
                            {
                                Identifier = untagIdent binding.Identifier
                                Expr = untagExpr binding.Expr
                            })
                Expr = untagExpr def.Expr
                Tag = ()
            }
        | Prim1Expr def ->
            Prim1Expr {
                Operator = def.Operator
                TypeArguments =
                    def.TypeArguments
                        |> List.map untagType
                Expr = untagExpr def.Expr
                Tag = ()
            }
        | Prim2Expr def ->
            Prim2Expr {
                Operator = def.Operator
                TypeArguments =
                    def.TypeArguments
                        |> List.map untagType
                Left = untagExpr def.Left
                Right = untagExpr def.Right
                Tag = ()
            }
        | IfExpr def ->
            IfExpr {
                Condition = untagExpr def.Condition
                TrueBranch = untagExpr def.TrueBranch
                FalseBranch = untagExpr def.FalseBranch
                Tag = ()
            }
        | NumberExpr def ->
            NumberExpr {
                Number = def.Number
                Tag = ()
            }
        | IdentifierExpr def ->
            IdentifierExpr {
                Name = def.Name
                Tag = ()
            }
        | BoolExpr def ->
            BoolExpr {
                Flag = def.Flag
                Tag = ()
            }
        | ApplicationExpr def ->
            ApplicationExpr {
                Identifier = untagIdent def.Identifier
                TypeArguments =
                    def.TypeArguments
                        |> List.map untagType
                Arguments =
                    def.Arguments |> List.map untagExpr
                Tag = ()
            }
        | AnnotationExpr def ->
            AnnotationExpr {
                Expr = untagExpr def.Expr
                Type = untagType def.Type
                Tag = ()
            }

    let untagDecl (decl : Decl<_>) =
        {
            Identifier =
                {
                    Name = decl.Identifier.Name
                    Tag = ()
                }
            Parameters =
                decl.Parameters
                    |> List.map untagIdent
            Body = untagExpr decl.Body
        }

    let untagProgram program =
        {
            Declarations =
                program.Declarations
                    |> List.map untagDecl
            Main = untagExpr program.Main
        }

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
                    |> Result.map untagProgram
            let msg = sprintf "Text: %s\nResult: %A" unparsed reparsed
            reparsed = Ok program |@ msg

        Check.One(config, parseUnparseIsOriginal)
