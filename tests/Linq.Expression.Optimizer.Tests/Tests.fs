
// https://fsprojects.github.io/FsUnit/
#if INTERACTIVE
#I "../../packages/test/xunit.extensibility.core/lib/portable-net45+win8+wp8+wpa81/"
#r "../../packages/test/xunit.extensibility.core/lib/portable-net45+win8+wp8+wpa81/xunit.core.dll"
#I "../../packages/test/xunit.assert/lib/dotnet/"
#r "../../packages/test/xunit.assert/lib/dotnet/xunit.assert.dll"
#I "../../packages/test/FsUnit.xUnit/lib/net45/"
#r "../../packages/test/FsUnit.xUnit/lib/net45/NHamcrest.dll"
#r "../../packages/test/FsUnit.xUnit/lib/net45/FsUnit.Xunit.dll"
#load "../../src/Linq.Expression.Optimizer/ExpressionOptimizer.fs"
#else
namespace Tests 
#endif

open System.Linq.Expressions
open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers
open NHamcrest.Core
open Xunit.Extensions

type ``Test Fixture`` () = 
    let executeExpression (e:Expression) =
        Expression.Lambda(e).Compile().DynamicInvoke() :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList

    let testExpression (qry: IQueryable<'b>) = 

        let optimized = ExpressionOptimizer.visit(qry.Expression)
        let expected = executeExpression qry.Expression
        if optimized.GetHashCode() <> qry.Expression.GetHashCode() then
            let actual = executeExpression optimized
            expected, actual
        else expected, expected

    let qry1 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            select (not(not(not(x>3))) && true)
        }

    let qry2 (arr:int list) =
        let q1 = query{
            for x in arr.AsQueryable() do
            where ((true && true) || (true && true))
            select (x+3)
        }
        query{ 
            for x in q1 do
            let x2 = (x-1)
            let x3 = (x+1)
            select (x2, x3)
        }
    let qry3 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            groupBy x into g
            select (g.Key, g.Count())
        }

    let qry4 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            sortByDescending x
            thenBy x
            select (if true then x else x)
        }

    let qry5 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            groupValBy x x into g
            select (g, g.Key, g.Count())
        }

    let qry6 (arr:int list) =
        query{
            for x1 in arr.AsQueryable() do
            join x2 in arr.AsQueryable() on (x1 = x2)
            select (x1, x2)
        }
    let qry7 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            let tmp = x
            select (not(not(not(x>3))) && true)
        }
    let qry8 (arr:int list) =
        query{
            for x1 in arr.AsQueryable() do
            groupJoin x2 in arr.AsQueryable()
                on ((x1+1) = x2) into g
            join x3 in arr.AsQueryable() on (x1 = x3)
            select (x1, x3)

        }
    let qry9 (arr:int list) =
        query{
            for x1 in arr.AsQueryable() do
            leftOuterJoin x2 in arr.AsQueryable() on (x1 = x2) into r
            for y in r.DefaultIfEmpty() do
            select (true, y, "asdf")
        }
    let qry10 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            where
                (query {
                    for y in arr.AsQueryable() do
                    exists (x = -y) })
            select (x)
        }
    let cond1 = 12
    let qry11 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            distinct
            skipWhile (cond1 < 3)
            skip 1
            take 4
            sortByNullable (Nullable(x))
        }

    let qry12 (arr:int list) =
        let f(v:int) = v
        query{
            for x in arr.AsQueryable() do
            where (cond1 = 12)
            where (cond1 = 12 && true && f(x) >  -1)
            let c = 2
            let y, y2 = cond1, x
            where (cond1 = 12)
            let tmp = y+c+f(x)
            where (tmp>1)
            sortByNullable (Nullable(x))
        }


    let qry13 (arr:int list) =
        query{
            for x in arr.AsQueryable() do
            where ((x, true) = (x, true) && true)
            select ((x>0 && x>1 && x>2 && x>3 && x>4 && x>0 && x>1 && x>2 && x>3 && x>4) ||
                (x<0 || x<1 || x<2 || x<3 || x<4 || x<0 || x<1 || x<2 || x<3 || x<4))
        }

    let testEq (xs:int[]) qry = 
        let res = xs |> Seq.toList |> qry |> testExpression
        res ||> should equal 
    
    let testLt (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        should lessThan (expr.ToString().Length) (optimized.ToString().Length)

    let testLteq (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        should lessThanOrEqualTo (expr.ToString().Length) (optimized.ToString().Length)


    [<Fact>]
    member test.``Expression optimizer generates equal results on 1-2-3-4-5 array`` () =
                    testEq [|1;2;3;4;5|] qry1

    [<Fact>]
    member test.``Expression optimizer generates smaller expression on 1-2-3-4-5 array`` () = 
                    testLt [|1;2;3;4;5|] qry1
        
    [<Property>]
    member test.``Expression optimizer generates equal results1`` (xs:int[]) = testEq xs qry1 
    [<Property>]
    member test.``Expression optimizer generates smaller expression1`` (xs:int[]) = testLt xs qry1

    [<Property>]
    member test.``Expression optimizer generates equal results2`` (xs:int[]) = testEq xs qry2 
    [<Property>]
    member test.``Expression optimizer generates smaller expression2`` (xs:int[]) = testLteq xs qry2

    [<Property>]
    member test.``Expression optimizer generates equal results3`` (xs:int[]) = testEq xs qry3
    [<Property>]
    member test.``Expression optimizer generates smaller expression3`` (xs:int[]) = testLteq xs qry3

    [<Property>]
    member test.``Expression optimizer generates equal results4`` (xs:int[]) = testEq xs qry4
    [<Property>]
    member test.``Expression optimizer generates smaller expression4`` (xs:int[]) = testLt xs qry4

    [<Property>]
    member test.``Expression optimizer generates equal results5`` () = testEq [|2;-2|] qry5
    [<Property>]
    member test.``Expression optimizer generates smaller expression5`` (xs:int[]) = testLteq xs qry5

    [<Property>]
    member test.``Expression optimizer generates equal results6`` (xs:int[]) = testEq xs qry6
    [<Property>]
    member test.``Expression optimizer generates smaller expression6`` (xs:int[]) = testLteq xs qry6

    [<Property>]
    member test.``Expression optimizer generates equal results7`` (xs:int[]) = testEq xs qry7 
    [<Property>]
    member test.``Expression optimizer generates smaller expression7`` (xs:int[]) = testLt xs qry7

    [<Property>]
    member test.``Expression optimizer generates equal results8`` (xs:int[]) = testEq xs qry8
    [<Property>]
    member test.``Expression optimizer generates smaller expression8`` (xs:int[]) = testLteq xs qry8

    [<Property>]
    member test.``Expression optimizer generates equal results9`` (xs:int[]) = testEq xs qry9
    [<Property>]
    member test.``Expression optimizer generates smaller expression9`` (xs:int[]) = testLteq xs qry9

    [<Property>]
    member test.``Expression optimizer generates equal results10`` (xs:int[]) = testEq xs qry10
    [<Property>]
    member test.``Expression optimizer generates smaller expression10`` (xs:int[]) = testLteq xs qry10

    [<Property>]
    member test.``Expression optimizer generates equal results11`` (xs:int[]) = testEq xs qry11
    [<Property>]
    member test.``Expression optimizer generates smaller expression11`` (xs:int[]) = testLt xs qry11

    [<Property>]
    member test.``Expression optimizer generates equal results12`` (xs:int[]) = testEq xs qry12
    [<Property>]
    member test.``Expression optimizer generates smaller expression12`` (xs:int[]) = testLteq xs qry12

    [<Property>]
    member test.``Expression optimizer generates equal results13`` (xs:int[]) = testEq xs qry13
    [<Property>]
    member test.``Expression optimizer generates smaller expression13`` (xs:int[]) = testLt xs qry13

