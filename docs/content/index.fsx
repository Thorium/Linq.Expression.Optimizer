(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Linq.Expression.Optimizer
======================

Documentation

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

# Example (C#)
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
            select !!!(x > 3) && true;

        var optimized = ExpressionOptimizer.visit(qry.Expression);

        Console.WriteLine(qry.Expression);
        Console.WriteLine(optimized);
        Console.ReadLine();
    }
}
```

# Example (F#)
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
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in docs/content/. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
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
