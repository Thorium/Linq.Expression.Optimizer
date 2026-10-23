/// What the optimizer promises of every tree: the optimized tree is the
/// same function (checked by compiling both and running them over a grid
/// of environments), optimizing never throws, optimizing twice is
/// optimizing once, and the tree never grows. A coverage test keeps the
/// generator honest: every reduction rule must fire on some sample.
module Linq.Expression.Optimizer.PropertyTests.OptimizerProperties

open System
open System.Linq.Expressions
open Xunit
open FsCheck.Xunit
open FsCheck
open FsCheck.FSharp
open Linq.Expression.Optimizer.PropertyTests.Trees

/// The number of nodes in a tree.
let private nodeCount (e: Expression) =
    let mutable n = 0
    let counter =
        { new ExpressionVisitor() with
            member this.Visit(node: Expression) =
                if not (isNull node) then n <- n + 1
                base.Visit node }
    counter.Visit e |> ignore
    n

let private optimize (e: Expression) : Result<Expression, string> =
    try
        Ok(ExpressionOptimizer.visit e)
    with ex -> Error $"{ex.GetType().Name}: {ex.Message}"

[<Property(MaxTest = 600)>]
let ``the optimized tree is the same function as the original`` () =
    Prop.forAll arbTree (fun tree ->
        let original = build tree
        match optimize original with
        | Error msg -> false |> Prop.label $"optimizing threw {msg}\n{original}"
        | Ok optimized ->
            match compile original, compile optimized with
            | Ok f, Ok g ->
                let differences =
                    envs
                    |> List.choose (fun env ->
                        let a = f (args env)
                        let b = g (args env)
                        if a = b then None else Some(env, a, b))
                let dropped = differences |> List.forall (fun (_, a, _) -> match a with Throws _ -> true | Value _ -> false)
                // An exception the original would have thrown (x / 0, null.Length) may
                // vanish when the optimizer removes the subtree that threw it, and
                // another throw site may then be the one that fires: the library is
                // documented for side-effect-free expressions, and a raised exception
                // is the one effect a pure expression can have. A changed value, or an
                // exception where the original had a value, is a defect.
                (List.isEmpty differences || dropped)
                |> Prop.label $"{original}\n--- optimized ---\n{optimized}\n--- differs on ---\n{differences}"
                |> Prop.classify (original.ToString() <> optimized.ToString()) "changed by the optimizer"
                |> Prop.classify (not (List.isEmpty differences) && dropped) "an exception was optimized away"
            | Ok _, Error msg -> false |> Prop.label $"the optimized tree does not compile: {msg}\n{original}\n--- optimized ---\n{optimized}"
            | Error msg, _ -> false |> Prop.label $"the original does not compile: {msg}\n{original}")

[<Property(MaxTest = 600)>]
let ``optimizing twice is optimizing once`` () =
    Prop.forAll arbTree (fun tree ->
        let original = build tree
        match optimize original with
        | Error msg -> false |> Prop.label msg
        | Ok once ->
            match optimize once with
            | Error msg -> false |> Prop.label $"the second pass threw {msg}\n{once}"
            | Ok twice ->
                (twice.ToString() = once.ToString())
                |> Prop.label $"{original}\n--- once ---\n{once}\n--- twice ---\n{twice}")

[<Property(MaxTest = 600)>]
let ``the optimized tree is never larger`` () =
    Prop.forAll arbTree (fun tree ->
        let original = build tree
        match optimize original with
        | Error msg -> false |> Prop.label msg
        | Ok optimized ->
            (nodeCount optimized <= nodeCount original)
            |> Prop.label $"{nodeCount original} nodes: {original}\n--- {nodeCount optimized} nodes ---\n{optimized}"
            |> Prop.classify (nodeCount optimized < nodeCount original) "smaller")

[<Property(MaxTest = 600)>]
let ``tryVisit is visit when visit succeeds`` () =
    Prop.forAll arbTree (fun tree ->
        let original = build tree
        match optimize original with
        | Error _ -> true |> Prop.label "visit threw, nothing to compare"
        | Ok optimized -> (ExpressionOptimizer.tryVisit(original).ToString() = optimized.ToString()) |> Prop.label (original.ToString()))

/// Every reduction rule fires on some generated tree. Run with the rule as
/// the only one, so a change by any other rule cannot mask a rule that
/// never matches the generator's shapes. (reductionMethods is a mutable
/// global; xunit runs the tests of one module sequentially, and the list
/// is put back whatever happens.)
[<Fact>]
let ``every reduction rule fires on some generated tree`` () =
    let all = ExpressionOptimizer.reductionMethods
    let samples = Gen.sampleWithSize 60 400 genTree |> Array.map build
    // ``remove AnonymousType`` wants a C# compiler anonymous type
    // (<>f__AnonymousType...), which F# cannot spell; the example suite covers it.
    let excused = [ 6 ]
    try
        let silent =
            all
            |> List.indexed
            |> List.filter (fun (i, _) -> not (List.contains i excused))
            |> List.choose (fun (i, rule) ->
                ExpressionOptimizer.reductionMethods <- [ rule ]
                let fires =
                    samples
                    |> Array.exists (fun e ->
                        try
                            (ExpressionOptimizer.visit e).ToString() <> e.ToString()
                        with _ -> false)
                if fires then None else Some i)
        Assert.True(List.isEmpty silent, "rules at positions " + String.Join(", ", silent) + " of reductionMethods never fired")
    finally
        ExpressionOptimizer.reductionMethods <- all

/// A rule whose rewrite holds the node it rewrote (x -> x + 0) still ends:
/// revisiting the rewrite meets that node again, and the visitor takes the
/// rewrite as it stands instead of reducing it in a loop. (reductionMethods
/// is put back as in the coverage test above.)
[<Fact>]
let ``a rule that wraps its input ends`` () =
    let all = ExpressionOptimizer.reductionMethods
    let wrap (e: Expression) =
        match e with
        | :? ParameterExpression as p when Type.(=)(p.Type, typeof<int>) -> Expression.Add(p, Expression.Constant 0) :> Expression
        | _ -> e
    try
        ExpressionOptimizer.reductionMethods <- [ wrap ]
        let optimized = ExpressionOptimizer.visit (Expression.Multiply(px, Expression.Constant 3))
        Assert.Equal("(((x + 0) + 0) * 3)", optimized.ToString())
    finally
        ExpressionOptimizer.reductionMethods <- all
