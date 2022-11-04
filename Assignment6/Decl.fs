namespace CompilerDesign.Assignment6

open CompilerDesign.Core

type Decl<'tag> =
    {
        /// Name of function begin declared.
        Identifier : IdentifierDef<'tag>
        Parameters : List<IdentifierDef<'tag>>
        Scheme : Scheme<'tag>
        Body : Expr<'tag>
    }

module Decl =

    let untag decl =
        {
            Identifier =
                {
                    Name = decl.Identifier.Name
                    Tag = ()
                }
            Parameters =
                decl.Parameters
                    |> List.map IdentifierDef.untag
            Scheme = Scheme.untag decl.Scheme
            Body = Expr.untag decl.Body
        }

    let private getSchemeTypes decl =
        match decl.Scheme.Type with
            | TypeArrow def -> def.InputTypes, def.OutputType
            | _ -> failwith "Unexpected"

    let unparse decl =
        let ident = decl.Identifier.Name
        let tvIdents =
            if decl.Scheme.Identifiers.IsEmpty then ""
            else
                decl.Scheme.Identifiers
                    |> Seq.map (fun ident -> $"'{ident.Name}")
                    |> String.concat ", "
                    |> sprintf "<%s>"
        let parmTypes, outType = getSchemeTypes decl
        let parms =
            (decl.Parameters, parmTypes)
                ||> Seq.map2 (fun ident typ ->
                    match typ with
                        | TypeBlank _ -> ident.Name
                        | _ -> $"{ident.Name} : {Type.unparse typ}")
                |> String.concat ", "
        let sOutType =
            match outType with
                | TypeBlank _ -> ""
                | _ -> $" -> {Type.unparse outType}"
        let body = Expr.unparse decl.Body
        $"def {ident}{tvIdents}({parms}){sOutType}:\n    {body}\n\n"

    let typeCheck env (decl : Decl<_>) =
        result {
            let! arrowDef =
                match decl.Scheme.Type with
                    | TypeArrow def ->
                        result {
                            do! Type.checkMissing def.OutputType
                            return def
                        }
                    | _ -> Error "Invalid decl scheme"

            let env' =
                (env, decl.Parameters, arrowDef.InputTypes)
                    |||> List.fold2 (fun acc ident typ ->
                            acc |> Map.add ident.Name typ)
            let! bodyType = Expr.typeOf env' decl.Body

            if bodyType <> arrowDef.OutputType then
                return! Type.mismatch arrowDef.OutputType bodyType
            else
                return Map.add
                    decl.Identifier.Name
                    decl.Scheme.Type
                    env
        }

type Program<'tag> =
    {
        Declarations : List<Decl<'tag>>
        Main : Expr<'tag>
    }

module Program =

    let untag program =
        {
            Declarations =
                program.Declarations
                    |> List.map Decl.untag
            Main = Expr.untag program.Main
        }

    let unparse program =
        let decls =
            program.Declarations
                |> List.map Decl.unparse
                |> String.concat ""
        let main = Expr.unparse program.Main
        $"{decls}{main}"

    let typeOf program =
        result {
            let program' = untag program
            let! env =
                (Map.empty, program'.Declarations)
                    ||> Result.List.foldM Decl.typeCheck
            return! Expr.typeOf env program'.Main
        }
