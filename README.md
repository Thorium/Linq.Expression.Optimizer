[![Issue Stats](http://issuestats.com/github/thorium/Linq.Expression.Optimizer/badge/issue)](http://issuestats.com/github/thorium/Linq.Expression.Optimizer)
[![Issue Stats](http://issuestats.com/github/thorium/Linq.Expression.Optimizer/badge/pr)](http://issuestats.com/github/thorium/Linq.Expression.Optimizer)

# Linq.Expression.Optimizer

Lightweight optimizer of System.Linq.Expression expressions. 
Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations. 
For side-effect free Expressions. No compilation-subjective optimizations. 
This is meant to be used with expressions that are not compiled but transferred to other domain.

Supported frameworks: Net 3.5, Net 4.5-... (and Mono), .NET Standard 1.6 (so also .NET Core)

## Supported optimizations

Example as a quote. There are various other cases also.

- Replace constants comparisons:    ` 3 < 4  ->  true ` 
- Remove anonymous types:    ` new AnonymousObject(Item1 = x, Item2 = "").Item1  -->  x `
- Cut not used condition:    ` if false then x else y  ->  y `
- Remove not:    ` not(false)  ->  true `
- Binary tree balancing:   ` a or (b or (c or (d or (e or (f or (g or h)))))) -> ((a or b) or (c or d)) or ((e or f) or (g or h)) `
- Captured closure constant ("free variable") evaluation:   ` y = 3 and (y + x)  ->  (3 + x) `
- Execute simple math: `5 * 3  ->  15` 
- Boolean algebra reductions:
  * gather            ` (x or y) and (x or z)   ->  x or (y and z) `  
  * identity          ` false or y  ->  y `              
  * annihilate        ` true or x  ->  true `             
  * absorb            ` x and (x or y)  ->  x `              
  * idempotence       ` y or y  ->  y `                      
  * complement        ` x and not(x)  ->  false `            
  * doubleNegation    ` not(not(y))  ->  y `                 
  * deMorgan          ` not(x) and not(y)  ->  not(x or y) ` 


## Background

This is a side-track from [SQLProvider](https://github.com/fsprojects/SQLProvider), excelent tool that is
kind of OR-mapper with auto-generated objects, so it compiles any databases to .NET-language, and works
fast on design time in Visual Studio or other editors.

But I needed better SQL-queries. So this optimses .NET LINQ-expressions.
These expressions were not ment to be compiled so [Nessos LinqOptimizer](https://github.com/nessos/LinqOptimizer) was not the right tool.
I thought that .NET would optimize these automatically, but no.
   
Read the [Getting started tutorial](http://thorium.github.io/Linq.Expression.Optimizer/index.html#Getting-started) to learn more.

Documentation: http://thorium.github.io/Linq.Expression.Optimizer

## Maintainer(s)

- [@thorium](https://github.com/thorium)

If you want more optimizations, please feel free to send PRs!
