namespace Assignment2

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type SyntaxFactory

type expr =
    | Num of int64
    | Add1 of expr
    | Sub1 of expr

module Expr =

    /// Corresponds to compile_expr in Lecture 3.
    /// https://course.ccs.neu.edu/cs4410sp22/lec_let-and-stack_notes.html
    let rec compile_expr = function
        | Num n ->
            LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal(n))
                :> Syntax.ExpressionSyntax
        | Add1 e ->
            BinaryExpression(
                SyntaxKind.AddExpression,
                compile_expr e,
                LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal(1)))
        | Sub1 e ->
            BinaryExpression(
                SyntaxKind.SubtractExpression,
                compile_expr e,
                LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal(1)))
