
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

open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers

open Xunit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open Xunit.Extensions
open Xunit.Abstractions

open NHamcrest.Core
open BenchmarkDotNet.Attributes

type Itm = {x:int}

module Queries =
    let executeExpression (e:Expression) =
        Expression.Lambda(e).Compile().DynamicInvoke() :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList

    let testExpression (qry: IQueryable<'b>) = 

        let optimized = ExpressionOptimizer.visit qry.Expression
        let expected = executeExpression qry.Expression
        if optimized.GetHashCode() <> qry.Expression.GetHashCode() then
            let actual = executeExpression optimized
            expected, actual
        else expected, expected

    let whereSelectLength (qryExpression: Expression) = 

        let o = qryExpression.ToString()
        let whereLength = 
            let startposW = o.IndexOf(".Where(") + 6
            if startposW = 5 then 
                0 
            else
                let endposW = o.IndexOf(").", startposW)
                if endposW = -1 then 
                    o.LastIndexOf(")") - startposW
                else endposW - startposW
        let selectLength = 
            let startposS = o.LastIndexOf(".Select(") + 7
            if startposS = 6 then 
                0 
            else
                let endposS = o.IndexOf(").", startposS)
                if endposS = -1 then 
                    o.LastIndexOf(")") - startposS
                else endposS - startposS

        whereLength, selectLength

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
    
    let qry11 (arr:int list) =
        let cond1 = 12
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
        let cond1 = 12
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


    let qry17 (arr:int list) =
        let arr2 = arr |> List.map(fun x -> x,x,x)
        query{
            for (x,y,z) in arr2.AsQueryable() do
            //      (x+10)                   +      y*6      +    (z - 2)      
            select ( ((((x+2)+2)+2)+ (2+2))  +  ((y*2)*3)    +   ((z - 1) - 1))
        }

    let qry18 (arr:int list) =
        let y : int Option = Option.Some 5
        let xx = box(Nullable<int>(7))
        query{
            for x in arr.AsQueryable() do
            where ((not ((xx :?> Nullable<int>).HasValue) || 
                    (xx :?> Nullable<int>).Value > 2) && (y.IsNone || (y.Value > x)) && true)
            select (1)
        }

    let qry19 (arr:int list) =

        let arr2 = arr |> List.map(fun x -> x, x < 5 , x > 2 && x < 7)
        query{
            for (i,a,b) in arr2.AsQueryable() do
            where (b || (not a && not b) || (a && not b))
            select i
        }

    let qry20 (arr:int list) =

        let arr2 = arr |> List.map(fun x -> x, x < 5 , x > 2 && x < 7,  x < 2 || x = 4 || x > 7)
        query{ // this is too deep, see issue #18
            for (i,a,b,c) in arr2.AsQueryable() do
            where ((not a && not b && not c) || (a && not b && not c) || (b && not c) || c)
            select i
        }

    let testEq (xs:int[]) qry = 
        let res = xs |> Seq.toList |> qry |> testExpression
        res ||> should equal 
    
    let testLt (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        let o = optimized.ToString()
        let o2 = o.ToString()
        printfn "%s" o2
        should lessThan (expr.ToString().Length) (optimized.ToString().Length)

    let testLteq (xs:int[]) qry = 
        let expr = xs |> Seq.toList |> qry |> (fun (q:IQueryable<'a>) -> q.Expression)
        let optimized = ExpressionOptimizer.visit(expr)
        should lessThanOrEqualTo (expr.ToString().Length) (optimized.ToString().Length)

open Queries
type ``Property Test Fixture`` () = 
    [<Fact>]
    member test.``Expression optimizer generates equal results on 1-2-3-4-5 array`` () =
                    testEq [|1;2;3;4;5|] qry1

    [<Fact>]
    member test.``Expression optimizer generates smaller expression on 1-2-3-4-5 array`` () = 
                    testLt [|1;2;3;4;5|] qry1
        
    [<Property>]
    member test.``Expression optimizer generates equal results01`` (xs:int[]) = testEq xs qry1 
    [<Property>]
    member test.``Expression optimizer generates smaller expression01`` (xs:int[]) = testLt xs qry1

    [<Property>]
    member test.``Expression optimizer generates equal results02`` (xs:int[]) = testEq xs qry2 
    [<Property>]
    member test.``Expression optimizer generates smaller expression02`` (xs:int[]) = testLteq xs qry2

    [<Property>]
    member test.``Expression optimizer generates equal results03`` (xs:int[]) = testEq xs qry3
    [<Property>]
    member test.``Expression optimizer generates smaller expression03`` (xs:int[]) = testLteq xs qry3

    [<Property>]
    member test.``Expression optimizer generates equal results04`` (xs:int[]) = testEq xs qry4
    [<Property>]
    member test.``Expression optimizer generates smaller expression04`` (xs:int[]) = testLt xs qry4

    [<Property>]
    member test.``Expression optimizer generates equal results05`` () = testEq [|2;-2|] qry5
    [<Property>]
    member test.``Expression optimizer generates smaller expression05`` (xs:int[]) = testLteq xs qry5

    [<Property>]
    member test.``Expression optimizer generates equal results06`` (xs:int[]) = testEq xs qry6
    [<Property>]
    member test.``Expression optimizer generates smaller expression06`` (xs:int[]) = testLteq xs qry6

    [<Property>]
    member test.``Expression optimizer generates equal results07`` (xs:int[]) = testEq xs qry7 
    [<Property>]
    member test.``Expression optimizer generates smaller expression07`` (xs:int[]) = testLt xs qry7

    [<Property>]
    member test.``Expression optimizer generates equal results08`` (xs:int[]) = testEq xs qry8
    [<Property>]
    member test.``Expression optimizer generates smaller expression08`` (xs:int[]) = testLteq xs qry8

    [<Property>]
    member test.``Expression optimizer generates equal results09`` (xs:int[]) = testEq xs qry9
    [<Property>]
    member test.``Expression optimizer generates smaller expression09`` (xs:int[]) = testLteq xs qry9

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

    [<Property>]
    member test.``Expression optimizer generates equal results17`` (xs:int[]) = testEq xs qry17
    [<Property>]
    member test.``Expression optimizer generates smaller expression17`` (xs:int[]) = testLteq xs qry17

    [<Property>]
    member test.``Expression optimizer generates equal results18`` (xs:int[]) = testEq xs qry18
    [<Property>]
    member test.``Expression optimizer generates smaller expression18`` (xs:int[]) = testLteq xs qry18

    [<Property>]
    member test.``Expression optimizer generates equal results19`` (xs:int[]) = testEq xs qry19
    [<Property>]
    member test.``Expression optimizer generates smaller expression19`` (xs:int[]) = testLt xs qry19

type ``Manual Test Fixture`` (output : ITestOutputHelper) = 
    let t = [1;2;3;4;5;6;7;8;9]

    let optQry (qry:int list -> IQueryable<_>) = 
        (qry t).Expression 
        |> ExpressionOptimizer.visit 

    let testLength (expectedWhere:int) (expectedSelect:int) exp =
        let actualWhere, actualSelect = whereSelectLength exp
        should equal expectedWhere actualWhere
        should equal expectedSelect actualSelect

    [<Fact>]
    member test.``qry01 optimized select where``() = 
        let exp = optQry qry1
        output.WriteLine (exp.ToString())
        testLength 0 18 exp

    [<Fact>]
    member test.``qry04 optimized select where``() = 
        let exp = optQry qry4
        output.WriteLine (exp.ToString())
        testLength 0 7 exp

    [<Fact>]
    member test.``qry07 optimized select where``() = 
        let exp = optQry qry7
        output.WriteLine (exp.ToString())
        testLength 0 40 exp

    [<Fact>]
    member test.``qry10 optimized select where``() = 
        let exp = optQry qry10
        output.WriteLine (exp.ToString())
        testLength 34 0 exp

    [<Fact>]
    member test.``qry11 optimized select where``() = 
        let exp = optQry qry11
        output.WriteLine (exp.ToString())
        testLength 0 0 exp

    [<Fact>]
    member test.``qry13 optimized select where``() = 
        let exp = optQry qry13
        output.WriteLine (exp.ToString())
        testLength 52 345 exp

    [<Fact>]
    member test.``qry14 optimized select where``() = 
        let exp = optQry qry14
        output.WriteLine (exp.ToString())
        testLength 10 18 exp

    [<Fact>]
    member test.``qry15 optimized select where``() = 
        let exp = optQry qry15
        output.WriteLine (exp.ToString())
        testLength 24 7 exp

    [<Fact>]
    member test.``qry17 optimized select where``() = 
        let exp = optQry qry17
        output.WriteLine (exp.ToString())
        testLength 0 88 exp

    [<Fact>]
    member test.``qry18 optimized select where``() = 
        let exp = optQry qry18
        output.WriteLine (exp.ToString())
        testLength 21 7 exp

    [<Fact>]
    member test.``qry19 optimized select where``() = 
        let exp = optQry qry19
        output.WriteLine (exp.ToString())
        testLength 18 29 exp

    [<Fact>]
    member test.``qry20 optimized select where``() = 
        let exp = optQry qry20
        output.WriteLine (exp.ToString())
        testLength 18 29 exp


    [<Fact>]
    member test.``can visit thousands of items in a fraction of a second``() = 

        let max =
#if DEBUG
            220 // Debug build is not tailcall optimized, this will take a second
#else
            5000 // Release build is optimized, this will not take a second
#endif

        let exps = 
            [| 0 .. max |] 
            |> Array.fold
                (fun s i -> Expression.And(s, Expression.LessThanOrEqual(Expression.Constant(i), Expression.Constant(i+1))) :> Expression)
                    (Expression.Constant(true) :> Expression)
        let sw = System.Diagnostics.Stopwatch.StartNew ()
        let opt = exps.Optimize()
        sw.Stop ()
        output.WriteLine ("Optimized: " + opt.ToString() + " Took " + sw.Elapsed.ToString())
        let exps2 = 
            [| 0 .. max |] 
            |> Array.fold
                (fun s i -> Expression.Or(s, Expression.Equal(Expression.Constant(i), Expression.Constant(i+1))) :> Expression)
                    (Expression.Constant(false) :> Expression)
        
        sw.Restart()
        let opt2 = exps2.Optimize()
        output.WriteLine ("Optimized 2: " + opt2.ToString() + " Took " + sw.Elapsed.ToString())


    [<Fact>]
    member test.``evaluate expression should match with replacement``() = 
        let Or' (a, b) = a || b
        let Not' = (not)
        let And' (a, b) = a && b
        let False' x = false
        let True' x = true

        for u in [true; false] do //u as underscore
        for p in [true; false] do
         for p1 in [true; false] do
          for p2 in [true; false] do
           for p3 in [true; false] do
            for p4 in [true; false] do
             for p5 in [true; false] do

               // Sequences taken from source

               let distribute = And' (p, Or' (p1, p2)) 
               let replacement = (p && p1) || (p && p2)
               should equal distribute replacement

               let gather = And' (Or'(p, p1), Or'(p2, p3))
               if (p = p2) then
                   let replacement = p || (p1 && p3)
                   should equal gather replacement

               let gather = And' (Or'(p, p1), Or'(p2, p3))
               if (p = p2) then
                   let replacement = p || (p1 && p3)
                   should equal gather replacement

               let absorb = And' (Or' (p1, u), p)
               if (p = p1) then
                   let replacement = p || (p1 && p3)
                   should equal absorb replacement

               let distribute_complement =

                   let wrap1 = And'
                   if (p = p1) then
                        let test1 exp =
                            let replacement = p && p2
                            wrap1 exp |> should equal replacement
                        test1 (p, Or' (Not' p1, p2))
                        test1 (p, Or' (p2, Not' p1))
                        test1 (Or' (p2, Not' p1), p)

                   let wrap2 = Or'
                   let test2 exp =
                       let replacement = p || p2
                       wrap2 exp |> should equal replacement
                   if (p = p1) then
                        test2 (p, And' (Not' p1, p2))
                        test2 (And' (p2, Not' p1), p)

                   let testTrue exp =
                       wrap2 exp |> should equal true

                   if (p = p1 && p2 = p4) then
                        testTrue (Or'(p2, Not' p), And'(p1, Not' p4))
                        testTrue (Or'(Not' p, p2), And'(p1, Not' p4))
                        testTrue (Or'(Not' p, p2), And'(Not' p4, p1))
                        testTrue (Or'(Not' p, p2), And'(Not' p4, p1))

                   let testNotp4 exp =
                       let replacement = not p4
                       wrap2 exp |> should equal replacement

                   if (p = p1 && p2 = p4) then
                   
                       testNotp4 (Not'(Or'(p, p2)), And'(p1, Not' p4))
                       testNotp4 (Not'(Or'(p2, p)), And'(p1, Not' p4))
                       testNotp4 (And'(p1, Not' p4), Not'(Or'(p, p2)))
                       testNotp4 (And'(p1, Not' p4), Not'(Or'(p2, p)))
                       testNotp4 (Not'(Or'(p, p2)), And'(Not' p4, p1))
                       testNotp4 (Not'(Or'(p2, p)), And'(Not' p4, p1))
                       testNotp4 (And'(Not' p4, p1), Not'(Or'(p, p2)))
                       testNotp4 (And'(Not' p4, p1), Not'(Or'(p2, p)))

                   if (p = p1 && p = p3) then
                        testTrue (Or' (p, And' (Not' p1, u)), Or'(Not' p3, u))
                        testTrue (Or' (p, And' (u, Not' p1)), Or'(Not' p3, u))
                        testTrue (Or' (And' (Not' p1, u), p), Or'(Not' p3, u))
                        testTrue (Or' (p, And' (Not' p1, u)), Or'(u, Not' p3))
                        testTrue (Or' (p, And' (u, Not' p1)), Or'(u, Not' p3))
                        testTrue (Or' (And' (Not' p1, u), p), Or'(u, Not' p3))
                        testTrue (Or' (And' (u, Not' p1), p), Or'(Not' p3, u))
                        testTrue (Or' (And' (u, Not' p1), p), Or'(u, Not' p3))
                        testTrue (Or'(Not' p3, u), Or' (p, And' (Not' p1, u)))
                        testTrue (Or'(Not' p3, u), Or' (p, And' (u, Not' p1)))
                        testTrue (Or'(Not' p3, u), Or' (And' (Not' p1, u), p))
                        testTrue (Or'(Not' p3, u), Or' (And' (u, Not' p1), p))
                        testTrue (Or'(u, Not' p3), Or' (p, And' (Not' p1, u)))
                        testTrue (Or'(u, Not' p3), Or' (p, And' (u, Not' p1)))
                        testTrue (Or'(u, Not' p3), Or' (And' (Not' p1, u), p))
                        testTrue (Or'(u, Not' p3), Or' (And' (u, Not' p1), p))
                   if (p = p1 && p2 = p4 && p2 = p5) then
                        testTrue (p5, Or'(Not'(Or'(p, p2)), And'(p1, Not' p4)))
                        testTrue (p5, Or'(Not'(Or'(p2, p)), And'(p1, Not' p4)))
                        testTrue (p5, Or'(Not'(Or'(p, p2)), And'(Not' p4, p1)))
                        testTrue (p5, Or'(Not'(Or'(p2, p)), And'(Not' p4, p1)))
                        testTrue (p5, Or'(And'(p1, Not' p4), Not'(Or'(p, p2))))
                        testTrue (p5, Or'(And'(p1, Not' p4), Not'(Or'(p2, p))))
                        testTrue (p5, Or'(And'(Not' p4, p1), Not'(Or'(p, p2))))
                        testTrue (p5, Or'(And'(Not' p4, p1), Not'(Or'(p2, p))))
                        testTrue (Or'(Not'(Or'(p, p2)), And'(p1, Not' p4)), p5)
                        testTrue (Or'(Not'(Or'(p2, p)), And'(p1, Not' p4)), p5)
                        testTrue (Or'(Not'(Or'(p, p2)), And'(Not' p4, p1)), p5)
                        testTrue (Or'(Not'(Or'(p2, p)), And'(Not' p4, p1)), p5)
                        testTrue (Or'(Not'(Or'(p, p2)), p5), And'(p1, Not' p4))
                        testTrue (Or'(Not'(Or'(p2, p)), p5), And'(p1, Not' p4))
                        testTrue (Or'(Not'(Or'(p, p2)), p5), And'(Not' p4, p1))
                        testTrue (Or'(Not'(Or'(p2, p)), p5), And'(Not' p4, p1))
                        testTrue (Or'(And'(p1, Not' p4), p5), Not'(Or'(p, p2)))
                        testTrue (Or'(And'(p1, Not' p4), p5), Not'(Or'(p2, p)))
                        testTrue (Or'(And'(Not' p4, p1), p5), Not'(Or'(p, p2)))
                        testTrue (Or'(And'(Not' p4, p1), p5), Not'(Or'(p2, p)))

                   if (p = p1 && p2 = p3) then
                        let testReplace exp =
                            let replacement = not p2
                            wrap2 exp |> should equal replacement

                        testReplace (And'(Not' p, Not' p2), And'(p1, Not' p3))
                        testReplace (And'(Not' p2, Not' p), And'(p1, Not' p3))
                        testReplace (And'(Not' p, Not' p2), And'(Not' p3, p1))
                        testReplace (And'(Not' p2, Not' p), And'(Not' p3, p1))
                        testReplace (And'(p1, Not' p3), And'(Not' p, Not' p2))
                        testReplace (And'(p1, Not' p3), And'(Not' p2, Not' p))
                        testReplace (And'(Not' p3, p1), And'(Not' p, Not' p2))
                        testReplace (And'(Not' p3, p1), And'(Not' p2, Not' p))

                   if (p = p1 && p = p2) then
                        let testReplace exp =
                            let replacement = (not p3) || p4 || p
                            wrap2 exp |> should equal replacement

                        testReplace (Or'(Not'(Or'(p3, p2)), And'(p4, Not' p1)), p)
                        testReplace (Or'(Not'(Or'(p2, p3)), And'(p4, Not' p1)), p)
                        testReplace (Or'(Not'(Or'(p3, p2)), And'(Not' p1, p4)), p)
                        testReplace (Or'(Not'(Or'(p2, p3)), And'(Not' p1, p4)), p)
                        testReplace (Or'(Not'(Or'(p3, p2)), p), And'(p4, Not' p1))
                        testReplace (Or'(Not'(Or'(p2, p3)), p), And'(p4, Not' p1))
                        testReplace (Or'(Not'(Or'(p3, p2)), p), And'(Not' p1, p4))
                        testReplace (Or'(Not'(Or'(p2, p3)), p), And'(Not' p1, p4))
                        testReplace (Or'(And'(p4, Not' p1), Not'(Or'(p3, p2))), p)
                        testReplace (Or'(And'(p4, Not' p1), Not'(Or'(p2, p3))), p)
                        testReplace (Or'(And'(Not' p1, p4), Not'(Or'(p3, p2))), p)
                        testReplace (Or'(And'(Not' p1, p4), Not'(Or'(p2, p3))), p)
                        testReplace (Or'(And'(p4, Not' p1), p), Not'(Or'(p3, p2)))
                        testReplace (Or'(And'(p4, Not' p1), p), Not'(Or'(p2, p3)))
                        testReplace (Or'(And'(Not' p1, p4), p), Not'(Or'(p3, p2)))
                        testReplace (Or'(And'(Not' p1, p4), p), Not'(Or'(p2, p3)))
                        testReplace (p, Or'(Not'(Or'(p3, p2)), And'(p4, Not' p1)))
                        testReplace (p, Or'(Not'(Or'(p2, p3)), And'(p4, Not' p1)))
                        testReplace (p, Or'(Not'(Or'(p3, p2)), And'(Not' p1, p4)))
                        testReplace (p, Or'(Not'(Or'(p2, p3)), And'(Not' p1, p4)))
                        testReplace (Or'(p, Not'(Or'(p3, p2))), And'(p4, Not' p1))
                        testReplace (Or'(p, Not'(Or'(p2, p3))), And'(p4, Not' p1))
                        testReplace (Or'(p, Not'(Or'(p3, p2))), And'(Not' p1, p4))
                        testReplace (Or'(p, Not'(Or'(p2, p3))), And'(Not' p1, p4))
                        testReplace (p, Or'(And'(p4, Not' p1), Not'(Or'(p3, p2))))
                        testReplace (p, Or'(And'(p4, Not' p1), Not'(Or'(p2, p3))))
                        testReplace (p, Or'(And'(Not' p1, p4), Not'(Or'(p3, p2))))
                        testReplace (p, Or'(And'(Not' p1, p4), Not'(Or'(p2, p3))))
                        testReplace (Or'(p, And'(p4, Not' p1)), Not'(Or'(p3, p2)))
                        testReplace (Or'(p, And'(p4, Not' p1)), Not'(Or'(p2, p3)))
                        testReplace (Or'(p, And'(Not' p1, p4)), Not'(Or'(p3, p2)))
                        testReplace (Or'(p, And'(Not' p1, p4)), Not'(Or'(p2, p3)))

                   let testRepl exp =
                        let replacement = (not p1) && (not p2) && (not p4 || p5)
                        wrap2 exp |> should equal replacement

                   if p2 = p3 && p1 = p then

                        testRepl (Not'(Or'(Or'(p2, p4), p1)), And'(And'(p5, Not' p3), Not' p))
                        testRepl (Not'(Or'(Or'(p2, p4), p1)), And'(And'(Not' p3, p5), Not' p))
                        testRepl (Not'(Or'(Or'(p2, p4), p1)), And'(Not' p, And'(p5, Not' p3)))
                        testRepl (Not'(Or'(Or'(p2, p4), p1)), And'(Not' p, And'(Not' p3, p5)))
                        testRepl (And'(And'(Not' p3, p5), Not' p), Not'(Or'(Or'(p4, p2), p1)))
                        testRepl (And'(And'(Not' p3, p5), Not' p), Not'(Or'(Or'(p2, p4), p1)))
                        testRepl (And'(Not' p, And'(p5, Not' p3)), Not'(Or'(Or'(p4, p2), p1)))
                        testRepl (And'(Not' p, And'(p5, Not' p3)), Not'(Or'(Or'(p2, p4), p1)))
                        testRepl (And'(Not' p, And'(Not' p3, p5)), Not'(Or'(Or'(p4, p2), p1)))
                        testRepl (And'(Not' p, And'(Not' p3, p5)), Not'(Or'(Or'(p2, p4), p1)))
                        testRepl (Not'(Or'(Or'(p4, p2), p1)), And'(And'(p5, Not' p3), Not' p))
                        testRepl (Not'(Or'(Or'(p4, p2), p1)), And'(And'(Not' p3, p5), Not' p))
                        testRepl (Not'(Or'(Or'(p4, p2), p1)), And'(Not' p, And'(p5, Not' p3)))
                        testRepl (Not'(Or'(Or'(p4, p2), p1)), And'(Not' p, And'(Not' p3, p5)))
                        testRepl (And'(And'(p5, Not' p3), Not' p), Not'(Or'(Or'(p4, p2), p1)))
                        testRepl (And'(And'(p5, Not' p3), Not' p), Not'(Or'(Or'(p2, p4), p1)))


                   ()
               let associate_complement =

                   let wrap1 = And'
                   if (p = p1) then
                       let test1 exp =
                            let replacement = p && (not p2)
                            wrap1 exp |> should equal replacement
                       test1 (Not' (And' (p, p2)), p1)
                       test1 (Not' (And' (p2, p)), p1)
                       test1 (p1, Not' (And' (p, p2)))
                       test1 (p1, Not' (And' (p2, p)))
                       let test2 exp =
                            let replacement = false
                            wrap1 exp |> should equal replacement
                       test2 (And' (p, u), Not' p1)
                       test2 (And' (Not' p, u), p1)
                       test2 (Not' p1, And' (p, u))
                       test2 (p1, And' (Not' p, u))

                   let wrap2 = Or'
                   if (p = p1) then
                       let test3 exp =
                            let replacement = p || (not p2)
                            wrap2 exp |> should equal replacement
                       test3 (Not' (Or' (p2, p)), p1)
                       test3 (Not' (Or' (p, p2)), p1)
                       test3 (p1, Not' (Or' (p, p2)))
                       test3 (p1, Not' (Or' (p2, p)))

                       let testTrue exp =
                           wrap2 exp |> should equal true
                       testTrue (Or' (u, p), Not' p1)
                       testTrue (Or' (p, u), Not' p1)
                       testTrue (Not' p1, Or' (u, p))
                       testTrue (Not' p1, Or' (p, u))
                       testTrue (Or' (u, Not' p), p1)
                       testTrue (Or' (Not' p, u), p1)
                       testTrue (p1, Or' (u, Not' p))
                       testTrue (p1, Or' (Not' p, u))

                       for u2 in [true; false] do //u as underscore

                           testTrue (Or'(u, Or' (u2, p)), Not' p1) 
                           testTrue (Or'(u, Or' (p, u2)), Not' p1) 
                           testTrue (Or'(Or' (p, u), u2), Not' p1) 
                           testTrue (Or'(Or' (u, p), u2), Not' p1) 
                           testTrue (Not' p1, Or'(u, Or' (u2, p))) 
                           testTrue (Not' p1, Or'(u, Or' (p, u2))) 
                           testTrue (Not' p1, Or'(Or' (p, u), u2)) 
                           testTrue (Not' p1, Or'(Or' (u, p), u2)) 
                           testTrue (Or' (Or'(u, Not' p), u2), p1) 
                           testTrue (Or' (Or'(Not' p, u), u2), p1) 
                           testTrue (p1, Or' (Or'(u, Not' p), u2)) 
                           testTrue (p1, Or' (Or'(Not' p, u), u2)) 
               ()


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
      (qry16 t).Expression;
      (qry17 t).Expression;
      (qry18 t).Expression;
      (qry19 t).Expression;
      (qry20 t).Expression;
      |]

  [<GlobalSetup>]
  member this.Setup() =()

  [<Benchmark(Baseline=true)>] 
  member this.ExecuteDirect() = 
    let x = queries |> Array.map executeExpression
    ()
  
  [<Benchmark>] 
  member this.ExecuteOpt1() = 
    let x = queries |> Array.map (ExpressionOptimizer.visit >> executeExpression)
    ()

module Starter = 
    let consoleLogger = 
        { new ITestOutputHelper with 
            member _.WriteLine x = Console.WriteLine x 
            member _.WriteLine(f,o) = failwith "not supported"
        }

    [<EntryPoint>]
    let main argv =
        Console.WriteLine "Testing a bit"
        ``Manual Test Fixture``(consoleLogger).``can visit thousands of items in a fraction of a second``()
        Console.WriteLine "Testing a bit2"

        let r = BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmark>()

        //let r = Benchmark().ExecuteOpt1()
        printfn "%O" r
        let _ = Console.ReadLine()
        0

// The main lag is the network transfer and SQL-execution.
// But we don't want the optimization to take too much resources.

//BenchmarkDotNet v0.13.12, Windows 11 (10.0.22631.4317/23H2/2023Update/SunValley3)
//13th Gen Intel Core i9-13900H, 1 CPU, 20 logical and 14 physical cores
//  [Host]     : .NET Framework 4.8.1 (4.8.9277.0), X64 LegacyJIT VectorSize=256

// Method        | Mean     | Error    | StdDev   | Ratio | Gen0    | Gen1    | Allocated | Alloc Ratio |
//|-------------- |---------:|---------:|---------:|------:|--------:|--------:|----------:|------------:|
//| ExecuteDirect | 11.14 ms | 0.128 ms | 0.120 ms |  1.00 | 78.1250 | 31.2500 | 561.53 KB |        1.00 |
//| ExecuteOpt1   | 11.58 ms | 0.102 ms | 0.091 ms |  1.04 | 93.7500 | 46.8750 | 657.88 KB |        1.17 |

// Result: A laptop ran 18 test cases in 0.00063 seconds.
