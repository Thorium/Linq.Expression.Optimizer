module Linq.Expression.Optimizer.Tests

open System.Linq.Expressions
open NUnit.Framework
open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers

let executeExpression (e:Expression) =
    Expression.Lambda(e).Compile().DynamicInvoke() :?> System.Collections.Generic.IEnumerable<bool>|> Seq.toList

[<Test>]
let ``Expression optimizer generates smaller expression tree and equal results`` () =

    let xs = [1;2;3;4;5].AsQueryable()
    let qry =
        query{
            for x in xs do
            select (not(not(not(x>3))) && true)
        }
    let optimized = ExpressionOptimizer. visit(qry.Expression)
    
    let expected = executeExpression qry.Expression
    let actual = executeExpression optimized

    CollectionAssert.AreEqual(expected, actual)
    Assert.LessOrEqual(optimized.ToString().Length,qry.Expression.ToString().Length)

// When FSCheck will be more mature, I'll write some of those tests here!
