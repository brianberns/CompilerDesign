﻿namespace CompilerDesign.Assignment6

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

    let getSignature decl =
        match decl.Scheme.Type with
            | TypeArrow arrowDef ->
                let typedParms = List.zip decl.Parameters arrowDef.InputTypes
                Ok (typedParms, arrowDef.OutputType)
            | _ -> Error "Invalid decl scheme"

    let unparse decl =
        let ident = decl.Identifier.Name
        let tvIdents = Scheme.unparseTypeVariableIdents decl.Scheme
        let typedParms, outputType =
            getSignature decl |> Result.get
        let parms =
            typedParms
                |> Seq.map (fun (ident, typ) ->
                    match typ with
                        | TypeBlank _ -> ident.Name
                        | _ -> $"{ident.Name} : {Type.unparse typ}")
                |> String.concat ", "
        let sOutputType =
            match outputType with
                | TypeBlank _ -> ""
                | _ -> $" -> {Type.unparse outputType}"
        let body = Expr.unparse decl.Body
        $"def {ident}{tvIdents}({parms}){sOutputType}:\n    {body}\n\n"

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
