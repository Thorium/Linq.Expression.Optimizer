// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions

#load "ExpressionOptimizer.fs"

let xs = [1;2;3;4;5].AsQueryable()

let cons = 8
let qry =
    query{
        for x in xs do
        select (not(not(not(x>3))) && true && cons>2)
    }

let optimized = ExpressionOptimizer.visit(qry.Expression)
qry.ToString()
optimized.ToString()
