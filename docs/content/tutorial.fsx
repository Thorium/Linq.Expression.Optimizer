(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
(*** hide ***)
#I "../../bin/Linq.Expression.Optimizer/"
(**
Introducing Linq.Expression.Optimizer
========================

## Some example of boolean algebra simplification / reduction:

*)
#if INTERACTIVE
#I "../../bin/Linq.Expression.Optimizer/"
#r "Linq.Expression.Optimizer.dll"
#endif
open System
open System.Linq

// e.g. Database IQueryable:
let xs = [1;2;3;4;5;6].AsQueryable()
let v = true

// Let's take some random horrible example:
let qry =
    query{
        for x in xs do
        let y = x+1
        where ((x>1 && (((x<2) && (x<2) && (v=false)) || (y>3) && (x<2))) 
              || (not(not(x < 2)) && ((v && not(v)) || (v)) || (v && (4>y))))
        select ((((x<2) && (x<2) && (v=false)) || (y>3) && (x<2)) 
                && (not(not(not(x>3))) && true) || (not(not(x < 2))))
    }

(**
Evaluating `qry.Expression.ToString()` will give a result:

```
[1; 2; 3; ... ]
    .Select(_arg1 => new AnonymousObject`2(Item1 = _arg1, Item2 = (_arg1 + 1)))
    .Where(tupledArg => (((tupledArg.Item1 > 1) AndAlso ((((tupledArg.Item1 < 2) 
            AndAlso (tupledArg.Item1 < 2)) AndAlso (FSI_0047.v == False)) OrElse 
            ((tupledArg.Item2 > 3) AndAlso (tupledArg.Item1 < 2)))) OrElse 
            ((Not(Not((tupledArg.Item1 < 2))) AndAlso ((FSI_0047.v AndAlso 
            Not(FSI_0047.v)) OrElse FSI_0047.v)) OrElse (FSI_0047.v AndAlso 
            (4 > tupledArg.Item2))))).Select(tupledArg => ((((((tupledArg.Item1 < 2)
            AndAlso (tupledArg.Item1 < 2)) AndAlso (FSI_0047.v == False)) 
            OrElse ((tupledArg.Item2 > 3) AndAlso (tupledArg.Item1 < 2))) 
            AndAlso (Not(Not(Not((tupledArg.Item1 > 3)))) AndAlso True)) 
            OrElse Not(Not((tupledArg.Item1 < 2)))))
```
*)

let optimized = ExpressionOptimizer.visit(qry.Expression)

(**
Evaluating `optimized.ToString()` will give a result:

```
  [1; 2; 3; ... ]
    .Select(_arg1 => new AnonymousObject`2(Item1 = _arg1, Item2 = (_arg1 + 1)))
    .Where(tupledArg => (((tupledArg.Item1 > 1) AndAlso ((tupledArg.Item2 > 3) 
        AndAlso (tupledArg.Item1 < 2))) OrElse ((tupledArg.Item1 < 2) OrElse 
        (4 > tupledArg.Item2)))).Select(tupledArg => ((((tupledArg.Item2 > 3) 
        AndAlso (tupledArg.Item1 < 2)) AndAlso Not((tupledArg.Item1 > 3))) 
        OrElse (tupledArg.Item1 < 2)))
```

So still bad, but not so bad.

## Anonymous object replacement

The way LINQ is constructed that methods like JOIN, WHERE, SELECT, GROUPBY, ... have all their own little lambda parameters.
If you use other expression tree visitors like [LinqKit](https://github.com/scottksmith95/LINQKit) these tools often merge 
lambdas to another lambdas to get bigger expression trees. Usually this is not visible to the user. But the end result is:
You endup in a situation where your SQL-query or whatever expression is massive. 

Let's see an example, with fairly simple LINQ-query:

*)

let qry = 
    query {
        for x in xs do
        let a = x.MyProperty1
        let b = x.MyProperty2
        select (b)
    }

(**

This will produce you three lamdas:
 
 - `_arg1 => new AnonymousObject(Item1=_arg1, Item2 = _arg1.MyProperty1, Item3 = _arg1.MyProperty2)`
 - `tupledarg => new AnonymousObject(Item1 = tupledarg.Item2, Item2 = tupledarg.Item3)`
 - `tupledarg => tupledarg.Item2`

The reason is that the earlier commands don't know what the later-ones wants so they provide new tuple with 
the whole object as first parameter.

Now, these tools just inject the mechanically the lambdas to other, resulting this:

```csharp
_arg1 => new AnonymousObject(
               Item1 = new AnonymousObject(
                             Item1=_arg1, 
                             Item2 = _arg1.MyProperty1, 
                             Item3 = _arg1.MyProperty2
                           ).Item2, 
               Item2 = new AnonymousObject(
                             Item1=_arg1, 
                             Item2 = _arg1.MyProperty1, 
                             Item3 = _arg1.MyProperty2
                           ).Item3
             ).Item2
```

Can you follow what happened?
And this is (kind of) correct. Now this expression will be transferred to the other domain, e.g. SQL. It depends the abilities of the O/R-mapper or what ever tool, that what this produces. But it can easily produce nested SELECT-clauses.

But...if you run this through this Linq.Expression.Optimizer, it will give the following result:

```csharp
// Middle step (not visible to user):
// _arg1 => new AnonymousObject(Item1 = _arg1.MyProperty1, Item2 = _arg1.MyProperty2).Item2

// Final result:
_arg1 => _arg1.MyProperty2
```

...and if you compare this to the original LINQ, you can see that this is exactly what you want!

For LinqKit users, there is a new [feature](https://github.com/scottksmith95/LINQKit#more-optimized-queries) that makes it easy to use this tool with LinqKit. Then if you want to, you can convert any of your EF IQueryables to optimized ones just by stating `.AsExpandable()` before your LINQ-logics. Useful if you have a large database or complex queries with a network-lag between business logics and database.

## How can I test that it produced correct result?

*)

open System
open System.Linq
open System.Collections.Generic
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers

let executeExpression (e:Expression) =
    Expression.Lambda(e).Compile().DynamicInvoke() 
    :?> System.Collections.Generic.IEnumerable<bool>|> Seq.toList

let test1 = executeExpression qry.Expression
let ensureCorrectness = executeExpression optimized
// both are: bool list = [true; false]

(**

More you can read from the [source code](https://github.com/Thorium/Linq.Expression.Optimizer/blob/master/src/Linq.Expression.Optimizer/ExpressionOptimizer.fs) which is pretty simple.

*)