namespace CompilerDesign.Assignment6

type IdentifierDef<'tag> =
    {
        Name : string
        Tag : 'tag
    }

type Type<'tag> =

    /// No type specified. Will be inferred later.
    | TypeBlank of 'tag

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of IdentifierDef<'tag>

    /// Type variable. E.g. "'a".
    | TypeVariable of IdentifierDef<'tag>

    /// Function type. E.g. "('a, Bool) -> Int".
    | TypeArrow of TypeArrowDef<'tag>

and TypeArrowDef<'tag> =
    {
        InputTypes : List<Type<'tag>>
        OutputType : Type<'tag>
        Tag : 'tag
    }

module Type =

    let rec unparse = function
        | TypeBlank _ -> "_"
        | TypeConstant def -> def.Name
        | TypeVariable def -> $"'{def.Name}"     // apostrophe is implicit
        | TypeArrow def ->
            let inputs =
                if def.InputTypes.IsEmpty then   // to-do: handle function with no inputs
                    "unit"
                else
                    def.InputTypes
                        |> Seq.map unparse
                        |> String.concat ", "
            $"({inputs} -> {unparse def.OutputType})"

/// E.g. const has scheme: ∀ab.a → b → a
type Scheme<'tag> =
    {
        Identifiers : List<IdentifierDef<'tag>>
        Type : Type<'tag>
        Tag : 'tag
    }
