namespace CompilerDesign.Assignment6

/// Generalized type signature of a function.
/// E.g. isnum has scheme: <'a>('a -> Bool)
type Scheme<'tag> =
    {
        /// E.g. <'a, 'b>.
        TypeVariableIdents : List<IdentifierDef<'tag>>

        /// E.g. ('a, 'b -> Bool).
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
