open CompilerDesign.Assignment6

(*
let text = "def j(): z k"
let parsed = Parser.parse text
*)
let parsed : Result<_, unit> =
    { Declarations =
       [{ Identifier = { Name = "j"
                         Tag = () }
          Parameters = [{ Name = "j"
                          Tag = () }]
          Scheme =
           { Identifiers = [{ Name = "d"
                              Tag = () }]
             Type =
              TypeArrow
                { InputTypes =
                   [TypeBlank ();
                    TypeArrow
                      { InputTypes =
                         [TypeVariable { Name = "x"
                                         Tag = () }; TypeConstant { Name = "z"
                                                                    Tag = () };
                          TypeVariable { Name = "q"
                                         Tag = () }]
                        OutputType = TypeConstant { Name = "a"
                                                    Tag = () }
                        Tag = () }]
                  OutputType = TypeConstant { Name = "p"
                                              Tag = () }
                  Tag = () }
             Tag = () }
          Body = AnnotationExpr { Expr = NumberExpr { Number = 0
                                                      Tag = () }
                                  Type = TypeConstant { Name = "q"
                                                        Tag = () }
                                  Tag = () } }]
      Main = IdentifierExpr { Name = "k"
                              Tag = () } } |> Ok

printfn "%A" parsed
match parsed with
    | Ok program ->
        printfn "%A" (Program.unparse program)
    | _ -> ()
