namespace Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

type prim1 =
    | Add1
    | Sub1

type expr<'a> =
    | Number of int64 * 'a
    | Prim1 of prim1 * expr<'a> * 'a

module Expr =

    let private numericLiteral (n : int64) =
        LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            Literal(n))

    /// Corresponds to compile_expr in Lecture 3.
    /// https://course.ccs.neu.edu/cs4410sp22/lec_let-and-stack_notes.html
    let rec compile_expr = function
        | Number (n, _) ->
            numericLiteral n
                :> Syntax.ExpressionSyntax
        | Prim1 (op, e, _) ->
            let kind =
                match op with
                    | Add1 -> SyntaxKind.AddExpression
                    | Sub1 -> SyntaxKind.SubtractExpression
            BinaryExpression(
                kind,
                compile_expr e,
                numericLiteral 1L)
