namespace Assignment1

type env = Map<string, int>

/// Question 1.
module Env =

    let empty : env = Map.empty

    let get (env : env) name =
        Map.tryFind name env

    let contains (env : env) name =
        Map.containsKey name env

    let add (env : env) name value =
        Map.add name value env

type arith =
    | Plus of arith * arith
    | Times of arith * arith
    | Variable of string
    | Num of int

/// Question 1.
module Arith =

    let rec evaluate arith env =
        match arith with
            | Plus (x, y) ->
                (evaluate x env) + (evaluate y env)
            | Times (x, y) ->
                (evaluate x env) * (evaluate y env)
            | Variable name ->
                match Env.get env name with
                    | Some value -> value
                    | None -> failwith $"No such variable: '{name}"
            | Num n -> n

    let rec pretty arith env =
        match arith with
            | Plus (x, y) ->
                $"({pretty x env} + {pretty y env})"
            | Times (x, y) ->
                $"({pretty x env} * {pretty y env})"
            | Variable name -> name
            | Num n -> string n
