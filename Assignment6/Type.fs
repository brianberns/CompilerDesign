namespace CompilerDesign.Assignment6

type Type<'tag> =

    /// No type specified. Will be inferred later.
    | TypeBlank of 'tag

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of string * 'tag

    /// Type variable. E.g. "'a".
    | TypeVariable of string * 'tag

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
        | TypeConstant (name, _) -> name
        | TypeVariable (name, _) -> name
        | TypeArrow def ->
            let inputs =
                def.InputTypes
                    |> Seq.map unparse
                    |> String.concat ", "
            $"({inputs}) -> {unparse def.OutputType}"
