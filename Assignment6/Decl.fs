namespace CompilerDesign.Assignment6

type Decl<'tag> =
    {
        /// Name of function begin declared.
        Identifier : IdentifierDef<'tag>
        Parameters : List<IdentifierDef<'tag>>
        Scheme : Scheme<'tag>
        Body : Expr<'tag>
    }

module Decl =

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
        $"def {ident}{tvIdents}({parms}){sOutType}:\n{body}"

type Program<'tag> =
    {
        Declarations : List<Decl<'tag>>
        Main : Expr<'tag>
    }

module Program =

    let unparse program =
        let decls =
            program.Declarations
                |> List.map Decl.unparse
                |> String.concat "\n"
        let main = Expr.unparse program.Main
        $"{decls}\n{main}"
