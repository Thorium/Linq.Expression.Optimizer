(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Linq.Expression.Optimizer
======================

Lightweight optimizer of System.Linq.Expression expressions. 
Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations. 
For side-effect free Expressions. No compilation-subjective optimizations. 
This is meant to be used with expressions that are not compiled but transferred to other domain.

Supported optimizations
-------

Example as a quote. There are various other cases also.

- Replace constants comparisons: ` 3 < 4  ->  true ` 
- Remove anonymous types: ` new AnonymousObject(Item1 = x, Item2 = "").Item1  ->  x `
- Cut not used condition: ` if false then x else y  ->  y `
- Remove not: ` not(false)  ->  true `
- Binary tree balancing: ` a or (b or (c or (d or (e or (f or (g or h)))))) -> ((a or b) or (c or d)) or ((e or f) or (g or h)) `
- Captured closure constant ("free variable") evaluation: ` y = 3 and (y + x)  ->  (3 + x) `
- Boolean algebra reductions:
  * gather            ` (x or y) and (x or z)   -> x or (y and z) `  
  * identity          ` false or y  ->  y `              
  * annihilate        ` true or x  ->  true `             
  * absorb            ` x and (x or y)  ->  x `              
  * idempotence       ` y or y  ->  y `                      
  * complement        ` x and not(x)  ->  false `            
  * doubleNegation    ` not(not(y))  ->  y `                 
  * deMorgan          ` not(x) and not(y)  ->  not(x or y) ` 


Installation
-------

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Linq.Expression.Optimizer library can be <a href="https://nuget.org/packages/Linq.Expression.Optimizer">installed from NuGet</a>:
      <pre>PM> Install-Package Linq.Expression.Optimizer</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Currently Linq.Expression.Optimizer is using .NET Framework 4.5.

Example (C#)
-------

This example demonstrates using a function defined in this sample library.

```csharp
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        // e.g. Database IQueryable:
        var xs = new[] { 1, 2, 3, 4, 5 }.AsQueryable();

        var qry =
            from x in xs
            select !!!((new { MyUnUsedItem = 123, 
                              MyItem = new Tuple<int, int>(x, 2).Item1
                            }.MyItem) > 3) && true;

        var optimized = ExpressionOptimizer.visit(qry.Expression);

        Console.WriteLine(qry.Expression);
            // System.Int32[].Select(x => (Not(Not(Not((
            // new <>f__AnonymousType0`2(MyUnUsedItem = 123, 
            // MyItem = new Tuple`2(x, 2).Item1).MyItem > 3)))) AndAlso True))

        Console.WriteLine(optimized);
            // System.Int32[].Select(x => Not((x > 3)))
        Console.ReadLine();

    }
}
```

Example (F#)
-------


*)
#if INTERACTIVE
#I "../../bin/Linq.Expression.Optimizer/"
#r "Linq.Expression.Optimizer.dll"
#endif

open System.Linq

// e.g. Database IQueryable:
let xs = [1;2;3;4;5].AsQueryable()

let qry =
    query{
        for x in xs do
        select (not(not(not(x>3))) && true)
    }
    
let optimized = ExpressionOptimizer.visit(qry.Expression)


(**

For more motivation why to use this, see: [Tutorial](tutorial.html)


Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in docs/content/. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.

   
Testimonials
--------------------------

This component is in use in a large scale enterprise application for simplifying and 
streamlining key financial processes. There is a huge data processing-pipeline with thousands of 
different SQL-clauses (some also very complex) against separate Oracle server cluster. 
The pipeline used pure EntityFramework and was converted to use LinqKit with Linq.Expression.Optimizer. 
The process was performance tested multiple times, and the execution time was dropped 
from 15h 30min to 14h 30min, mainly due to delivering less CPU usage to Oracle servers.
 
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/thorium/Linq.Expression.Optimizer/tree/master/docs/content
  [gh]: https://github.com/thorium/Linq.Expression.Optimizer
  [issues]: https://github.com/thorium/Linq.Expression.Optimizer/issues
  [readme]: https://github.com/thorium/Linq.Expression.Optimizer/blob/master/README.md
  [license]: https://github.com/thorium/Linq.Expression.Optimizer/blob/master/LICENSE.txt
*)
