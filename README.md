[![Issue Stats](http://issuestats.com/github/thorium/Linq.Expression.Optimizer/badge/issue)](http://issuestats.com/github/thorium/Linq.Expression.Optimizer)
[![Issue Stats](http://issuestats.com/github/thorium/Linq.Expression.Optimizer/badge/pr)](http://issuestats.com/github/thorium/Linq.Expression.Optimizer)

# Linq.Expression.Optimizer

Lightweight optimizer of System.Linq.Expression expressions. 
Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations. 
For side-effect-free Expressions. No compilation-subjective optimizations. 
This is meant to be used with expressions that are not compiled but transferred to other domains.

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

## What a rewrite keeps

The optimized expression gives the value of the original for every input and throws only where the
original throws. Optimizing a valid tree does not throw.

- **Evaluation order.** A rewrite may evaluate fewer operands, but never one the original would have
  skipped, and the operands that stay keep their left-to-right order: a guard stays ahead of what it
  guards (` o != null and o.Flag `). An exception the original would throw may disappear with a
  subtree that is optimized away.
- **The same operand.** Two operands are one only when they are the same parameter (by reference, not
  by name), constants holding the same reference (the same value for value types and strings), or the
  same member chain over those. Anything holding a method call, an Invoke, a New or an index is never
  the same as another.
- **Arithmetic.** Constants are regrouped (` (x + 2) + 3  ->  x + 5 `) for integral types in unchecked
  nodes only. Identities are applied where they are exact: not ` x + 0.0 ` (it turns -0.0 into 0.0),
  not ` x * 0 ` or ` x % 1 ` on a Nullable. A decimal is the same by value: its constants are folded and
  its identities applied, and a result may carry another scale than the original's (` 6.0 ` for ` 6.00 `).
  A fold that overflows is left for the run time.
- **Operators.** A node is folded or rewritten only when its operator is the built-in one: no `Method`;
  an `op_*` method of string, decimal, DateTime, DateTimeOffset, TimeSpan, Guid or BigInteger (also the
  static `String.Concat` and `String.Equals`); or an F# operator (`Operators.op_*`,
  `LanguagePrimitives.*Dynamic`) over those types, primitives, enums, and options, lists, tuples and
  Nullables of them. Objects are compared by reference, never by value.
- **Comparison ranges** (` x > 1 and x > 2  ->  x > 2 `) are left alone over Nullable operands and NaN constants.
- **Tuples and anonymous types.** A tuple is a System.Tuple, a System.ValueTuple or an AnonymousObject;
  a member of an anonymous type is found through `NewExpression.Members`.
- **Other nodes.** A node type without a rule (Default, Index, Switch, Try, ...) stays as it is.
- **Deep trees.** A tree nested deeper than the stack has room for is optimized down to that depth and
  left as it is below, instead of overflowing the stack.
- **Captured values.** A captured closure constant is read when the expression is optimized: the
  optimized expression does not see a later change of the captured variable.


## Background

This is a side-track from [SQLProvider](https://github.com/fsprojects/SQLProvider), excellent tool that is
kind of OR-mapper with auto-generated objects, so it compiles any databases to .NET-language, and works
fast on design time in Visual Studio or other editors.

But I needed better SQL-queries. So this optimises .NET LINQ-expressions.
These expressions were not meant to be compiled, so [Nessos LinqOptimizer](https://github.com/nessos/LinqOptimizer) was not the right tool.
I thought that .NET would optimize these automatically, but no.
   
Read the [Getting started tutorial](http://thorium.github.io/Linq.Expression.Optimizer/index.html#Getting-started) to learn more.

Documentation: http://thorium.github.io/Linq.Expression.Optimizer

## Tests

`tests/Linq.Expression.Optimizer.Tests` is the example-based suite over LINQ queries.
`tests/Linq.Expression.Optimizer.PropertyTests` is an FsCheck suite over random expression
trees (booleans, integers, doubles with NaN, decimals, nullable integers, strings and an object
that may be null over fifteen parameters, two of them named alike and two unnamed, in the shapes
every reduction rule is written for): the optimized tree
compiles to the same function as the original on a grid of inputs, optimizing never throws,
optimizing twice is optimizing once, the tree never grows, and every reduction rule fires
on some generated tree. An exception the original would throw (`x / 0`, `null.Length`) may
disappear when the subtree that threw it is optimized away - the library is for
side-effect-free expressions - but a value never changes and no new exception appears.

## Maintainer(s)

- [@thorium](https://github.com/thorium)

If you want more optimizations, please feel free to send PRs!
