namespace CompilerDesign.Assignment6

/// Generalized type signature of a function.
/// E.g. isnum has scheme: <'a>('a -> Bool)
[<System.Diagnostics.DebuggerDisplay("{Unparse()}")>]
type Scheme<'tag> =
    {
        /// E.g. <'a, 'b>.
        TypeVariableIdents : List<IdentifierDef<'tag>>

        /// E.g. ('a, 'b -> Bool).
        Type : Type<'tag>

        Tag : 'tag
    }

    with

    member scheme.UnparseTypeVariableIdents() =
        if scheme.TypeVariableIdents.IsEmpty then ""
        else
            scheme.TypeVariableIdents
                |> Seq.map (fun ident -> $"'{ident.Name}")
                |> String.concat ", "
                |> sprintf "<%s>"

    member scheme.Unparse() =
        let typeVars = scheme.UnparseTypeVariableIdents()
        $"{typeVars}{Type.unparse scheme.Type}"

module Scheme =

    let untag scheme =
        {
            TypeVariableIdents =
                scheme.TypeVariableIdents
                    |> List.map IdentifierDef.untag
            Type = Type.untag scheme.Type
            Tag = ()
        }

    let unparseTypeVariableIdents (scheme : Scheme<_>) =
        scheme.UnparseTypeVariableIdents()

    let unparse (scheme : Scheme<_>) =
        scheme.Unparse()
