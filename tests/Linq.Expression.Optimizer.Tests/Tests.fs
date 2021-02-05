
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
open BenchmarkDotNet.Attributes

type Itm = {x:int}

module Queries =
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

    let qry14 (arr:int list) =
        let aq = arr.AsQueryable()
        query{
            for x in arr.AsQueryable() do
            where (arr = arr)
            where (aq = aq.Reverse().Reverse().AsQueryable())
            where (arr = [1; 2; 3])
            select (not(not(not(x>3))) && true)
        }

    let qry15 (arr:int list) =
        let y : int Option = Option.None
        let xx = box(Nullable<int>())
        query{
            for x in arr.AsQueryable() do
            where ((not ((xx :?> Nullable<int>).HasValue) || 
                    (xx :?> Nullable<int>).Value > 2) && (y.IsNone || (y.Value > x)) && true)
            select (1)
        }
    
    let qry16 (arr:int list) =
        let toImts x = {x = x}
        let asItms = arr |> List.map(toImts)
        let arr2 = asItms.AsQueryable()
        let onlyX = true
        query{
            for i in arr2 do
            join j in arr2 on (i.x = j.x)
            where (((not onlyX) ||
                      (onlyX && i.x=3)) && arr2.Any(fun sl -> false || sl.x = j.x && j.x <> 1))
            select (i.x, j.x)
        }

    let testEq (xs:int[]) qry = 
        let res = xs |> Seq.toList |> qry |> testExpression
        res ||> should equal 
    
    let testLt (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        let o = optimized.ToString()
        let o2 = o.ToString()
        should lessThan (expr.ToString().Length) (optimized.ToString().Length)

    let testLteq (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        should lessThanOrEqualTo (expr.ToString().Length) (optimized.ToString().Length)

open Queries
type ``Test Fixture`` () = 
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

    [<Property>]
    member test.``Expression optimizer generates equal results14`` (xs:int[]) = testEq xs qry14
    [<Property>]
    member test.``Expression optimizer generates smaller expression14`` (xs:int[]) = testLt xs qry14

    [<Property>]
    member test.``Expression optimizer generates equal results15`` (xs:int[]) = testEq xs qry15
    [<Property>]
    member test.``Expression optimizer generates smaller expression15`` (xs:int[]) = testLt xs qry15

    [<Property>]
    member test.``Expression optimizer generates equal results16`` (xs:int[]) = testEq xs qry16
    [<Property>]
    member test.``Expression optimizer generates smaller expression16`` (xs:int[]) = testLteq xs qry16

[<MemoryDiagnoser>]
type Benchmark() =
  let t = [1;2;3;4;5;6;7;8;9]
  let queries = [|
      (qry1 t).Expression;
      (qry2 t).Expression; 
      (qry3 t).Expression; 
      (qry4 t).Expression; 
      (qry5 t).Expression; 
      (qry6 t).Expression; 
      (qry7 t).Expression; 
      (qry8 t).Expression; 
      (qry9 t).Expression; 
      (qry10 t).Expression; 
      (qry11 t).Expression; 
      (qry12 t).Expression; 
      (qry13 t).Expression; 
      (qry14 t).Expression; 
      (qry15 t).Expression; 
      (qry16 t).Expression |]
  let visitAndExecute = ExpressionOptimizer.visit >> executeExpression

  [<GlobalSetup>]
  member this.Setup() =()

  [<Benchmark(Baseline=true)>] 
  member this.ExecuteDirect() = 
    let x = queries |> Array.map executeExpression
    ()
  
  [<Benchmark>] 
  member this.ExecuteOpt1() = 
    let x = queries |> Array.map visitAndExecute
    ()

module Starter = 
    [<EntryPoint>]
    let main argv =
        let r = BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmark>()
        printfn "%O" r
        0

// The main lag is the network transfer and SQL-execution.
// But we don't want the optimization to take too much resourses.

//|       Method |     Mean |    Error |   StdDev | Ratio | RatioSD |    Gen 0 |   Gen 1 | Gen 2 | Allocated |
//|------------- |---------:|---------:|---------:|------:|--------:|---------:|--------:|------:|----------:|
//| ExecutDirect | 16.13 ms | 0.318 ms | 0.340 ms |  1.00 |    0.00 |  93.7500 | 31.2500 |     - | 490.39 KB |
//|  ExecuteOpt1 | 17.04 ms | 0.160 ms | 0.149 ms |  1.06 |    0.02 | 125.0000 | 62.5000 |     - | 606.01 KB |

