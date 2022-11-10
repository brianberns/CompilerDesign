namespace CompilerDesign.Assignment6

/// E.g. isnum has scheme: <'a>('a -> Bool)
type Scheme<'tag> =
    {
        TypeVariableIdents : List<IdentifierDef<'tag>>
        Type : Type<'tag>
        Tag : 'tag
    }

module Scheme =

    let untag scheme =
        {
            TypeVariableIdents =
                scheme.TypeVariableIdents
                    |> List.map IdentifierDef.untag
            Type = Type.untag scheme.Type
            Tag = ()
        }

    let unparseTypeVariableIdents scheme =
        if scheme.TypeVariableIdents.IsEmpty then ""
        else
            scheme.TypeVariableIdents
                |> Seq.map (fun ident -> $"'{ident.Name}")
                |> String.concat ", "
                |> sprintf "<%s>"

    let unparse scheme =
        let typeVars = unparseTypeVariableIdents scheme
        $"{typeVars}{Type.unparse scheme.Type}"

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

    let private getSchemeTypes decl =
        match decl.Scheme.Type with
            | TypeArrow def -> def.InputTypes, def.OutputType
            | _ -> failwith "Unexpected"

    let unparse decl =
        let ident = decl.Identifier.Name
        let tvIdents = Scheme.unparseTypeVariableIdents decl.Scheme
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

type Program<'tag> =
    {
        DeclGroups : List<DeclGroup<'tag>>
        Main : Expr<'tag>
    }

module Program =

    let untag program =
        {
            DeclGroups =
                program.DeclGroups
                    |> List.map DeclGroup.untag
            Main = Expr.untag program.Main
        }

    let unparse program =
        let declGroups =
            program.DeclGroups
                |> List.map DeclGroup.unparse
                |> String.concat ""
        let main = Expr.unparse program.Main
        $"{declGroups}{main}"
