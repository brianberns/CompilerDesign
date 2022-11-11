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
                IdentifierDef.create decl.Identifier.Name
            Parameters =
                decl.Parameters
                    |> List.map IdentifierDef.untag
            Scheme = Scheme.untag decl.Scheme
            Body = Expr.untag decl.Body
        }

    let getTypeArrow decl =
        match decl.Scheme.Type with
            | TypeArrow arrowDef -> Ok arrowDef
            | _ -> Error "Invalid decl scheme"

    let unparse decl =
        let ident = decl.Identifier.Name
        let tvIdents = Scheme.unparseTypeVariableIdents decl.Scheme
        let parmTypes, outType =
            let arrowDef =
                decl
                    |> getTypeArrow
                    |> Result.get
            arrowDef.InputTypes, arrowDef.OutputType
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

type DeclGroup<'tag> =
    {
        Decls : List<Decl<'tag>>
    }

module DeclGroup =

    let untag group =
        {
            Decls =
                List.map Decl.untag group.Decls
        }

    let unparse group =
        group.Decls
            |> List.map Decl.unparse
            |> String.concat "and "
