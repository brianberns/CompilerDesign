namespace CompilerDesign.Assignment6

/// The type of a value or function.
type Type<'tag> =

    /// No type specified. Will be inferred later.
    | TypeBlank of 'tag

    /// Type constant. E.g. "Int", "Bool".
    | TypeConstant of IdentifierDef<'tag>

    /// Type variable. E.g. "'a".
    | TypeVariable of IdentifierDef<'tag>

    /// Function type. E.g. "('a, Bool) -> Int".
    | TypeArrow of TypeArrowDef<'tag>

/// Function type. E.g. "('a, Bool) -> Int".
and TypeArrowDef<'tag> =
    {
        InputTypes : List<Type<'tag>>
        OutputType : Type<'tag>
        Tag : 'tag
    }

module Type =

    let rec untag = function
        | TypeBlank _ -> TypeBlank ()
        | TypeConstant def -> TypeConstant (IdentifierDef.untag def)
        | TypeVariable def -> TypeVariable (IdentifierDef.untag def)
        | TypeArrow def ->
            TypeArrow {
                InputTypes =
                    def.InputTypes |> List.map untag
                OutputType = untag def.OutputType
                Tag = ()
            }

    let rec unparse = function
        | TypeBlank _ -> "_"
        | TypeConstant def -> def.Name
        | TypeVariable def -> $"'{def.Name}"     // apostrophe is implicit
        | TypeArrow def ->
            let inputs =
                if def.InputTypes.IsEmpty then
                    "Unit"                       // no other way to represent a function with no inputs
                else
                    def.InputTypes
                        |> Seq.map unparse
                        |> String.concat ", "
            $"({inputs} -> {unparse def.OutputType})"

    let rec freeTypeVars = function
        | TypeVariable ident -> Set.singleton ident
        | TypeArrow def ->
            List.fold (fun ftvs typ ->
                Set.union (freeTypeVars typ) ftvs)
                (freeTypeVars def.OutputType)
                def.InputTypes
        | _ -> Set.empty

    let int = TypeConstant (IdentifierDef.create "Int")
    let bool = TypeConstant (IdentifierDef.create "Bool")
