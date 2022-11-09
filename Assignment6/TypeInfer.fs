namespace CompilerDesign.Assignment6

open CompilerDesign.Core
open Substitution

type private SchemeEnvironment =
    Map<string, Scheme<unit>>

module SchemeEnvironment =

    let apply (subst : Substitution<_>) (env : SchemeEnvironment) =
        (env, subst)
            ||> List.fold (fun acc (fromIdent, toType) ->
                Map.map (fun _ typ ->
                    Scheme.substitute fromIdent toType typ) acc)

    let initial : SchemeEnvironment =
        [
            Prim1.unparse Add1, "(Int -> Int)"
            Prim1.unparse Sub1, "(Int -> Int)"
            Prim1.unparse Print, "<'a>('a -> 'a)"
            Prim1.unparse IsBool, "<'a>('a -> Bool)"
            Prim1.unparse IsNum, "<'a>('a -> Bool)"
            Prim1.unparse Not, "(Bool -> Bool)"

            Prim2.unparse Plus, "(Int, Int -> Int)"
            Prim2.unparse Minus, "(Int, Int -> Int)"
            Prim2.unparse Times, "(Int, Int -> Int)"
            Prim2.unparse And, "(Bool, Bool -> Bool)"
            Prim2.unparse Or, "(Bool, Bool -> Bool)"
            Prim2.unparse Greater, "(Int, Int -> Bool)"
            Prim2.unparse GreaterEq, "(Int, Int -> Bool)"
            Prim2.unparse Less, "(Int, Int -> Bool)"
            Prim2.unparse LessEq, "(Int, Int -> Bool)"
            Prim2.unparse Eq, "<'a>('a, 'a -> Bool)"
        ]
            |> List.map (fun (name, text) ->
                result {
                    let! scheme = Parser.Scheme.parse text
                    return name, Scheme.untag scheme
                })
            |> Result.List.sequence
            |> Result.get
            |> Map

    let tryFind name (env : SchemeEnvironment) =
        env
            |> Map.tryFind name
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error $"Name not found: {name}")

    let tryFindIdent ident =
        tryFind ident.Name

    let tryFindPrim1 prim1 =
        tryFind (Prim1.unparse prim1)

    let tryFindPrim2 prim2 =
        tryFind (Prim2.unparse prim2)

module TypeInfer =

    let private generateSymbol =
        let mutable count = 0
        fun (str : string) ->
            count <- count + 1
            $"{str}_{count}"

    let private generateTypeVariable str =
        generateSymbol str
            |> IdentifierDef.create
            |> TypeVariable

    module Scheme =

        let instantiate scheme =
            let subst =
                scheme.TypeVariableIdents
                    |> List.map (fun ident ->
                        let tv =
                            TypeVariable {
                                Name = generateSymbol ident.Name
                                Tag = scheme.Tag
                            }
                        ident, tv)
            Type.apply subst scheme.Type

    let (++) = Substitution.compose

    let rec private inferExpr funenv env = function
        | NumberExpr _ -> Ok (Substitution.empty, Type.int)
        | BoolExpr _ -> Ok (Substitution.empty, Type.bool)
        | IdentifierExpr ident -> inferIdentifier funenv env ident
        | Prim1Expr def -> inferPrim1 funenv env def
        | Prim2Expr def -> inferPrim2 funenv env def
        | IfExpr def -> inferIf funenv env def
        | LetExpr def -> inferLet funenv env def
        | ApplicationExpr def -> inferApplication funenv env def
        | AnnotationExpr def -> inferExpr funenv env def.Expr

    and private inferIdentifier _funenv env ident =
        result {
            let! typ = TypeEnvironment.tryFind ident env
            return Substitution.empty, typ
        }

    and private inferPrim1 funenv env (def : Prim1Def<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindPrim1 def.Operator funenv
            let schemeType = Scheme.instantiate scheme
            let! argSubst, argType =
                inferExpr funenv env def.Expr
            let outType = generateTypeVariable "out"
            let arrowType =
                TypeArrow {
                    InputTypes = [argType]
                    OutputType = outType
                    Tag = ()
                }
            let! subst = unify schemeType arrowType
            return
                argSubst ++ subst,
                Type.apply subst outType
        }

    and private inferPrim2 funenv env (def : Prim2Def<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindPrim2 def.Operator funenv
            let schemeType = Scheme.instantiate scheme
            let! leftSubst, leftType =
                inferExpr funenv env def.Left
            let! rightSubst, rightType =
                inferExpr funenv env def.Right
            let outType = generateTypeVariable "out"
            let arrowType =
                TypeArrow {
                    InputTypes = [leftType; rightType]
                    OutputType = outType
                    Tag = ()
                }
            let! subst = unify schemeType arrowType
            return
                leftSubst ++ rightSubst ++ subst,
                Type.apply subst outType
        }

    and private inferIf funenv env (def : IfDef<_>) =
        result {
            let! condSubst, condType =
                inferExpr funenv env def.Condition
            let! trueSubst, trueType =
                inferExpr funenv env def.TrueBranch
            let! falseSubst, falseType =
                inferExpr funenv env def.Condition
            let! boolSubst = unify condType Type.bool
            let! sameSubst = unify trueType falseType
            return
                condSubst ++ trueSubst ++ falseSubst
                    ++ boolSubst ++ sameSubst,
                Type.apply sameSubst trueType
        }

    and private inferLet funenv env (def : LetDef<_>) =
        result {
            let! env', bindingSubst =
                ((env, Substitution.empty), def.Bindings)
                    ||> Result.List.foldM (fun (accEnv, accSubst) binding ->
                        result {
                            let! subst, typ =
                                inferExpr funenv accEnv binding.Expr
                            let accEnv' = Map.add binding.Identifier typ accEnv
                            return accEnv', accSubst ++ subst
                        })
            let! bodySubst, bodyType = inferExpr funenv env' def.Expr
            return
                bindingSubst ++ bodySubst,
                Type.apply bodySubst bodyType
        }

    and private inferApplication funenv env (def : ApplicationDef<_>) =
        result {
            let! scheme =
                SchemeEnvironment.tryFindIdent def.Identifier funenv
            let schemeType = Scheme.instantiate scheme
            let! argSubsts, argTypes =
                def.Arguments
                    |> Result.List.traverse (
                        inferExpr funenv env)
                    |> Result.map List.unzip
            let outType = generateTypeVariable "out"
            let arrowType =
                TypeArrow {
                    InputTypes = argTypes
                    OutputType = outType
                    Tag = ()
                }
            let! subst = unify schemeType arrowType
            return
                (List.reduce (++) argSubsts) ++ subst,
                Type.apply subst outType
        }

    let inferType expr =
        result {
            let expr' = Expr.untag expr
            let! _, typ =
                inferExpr
                    SchemeEnvironment.initial
                    Map.empty
                    expr'
            return typ
        }
