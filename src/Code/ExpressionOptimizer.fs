/// This is just a light-weight expression optimizer.
/// It won't do any heavy stuff...
module ExpressionOptimizer

open System.Linq.Expressions
open System
open System.Reflection


open Microsoft.FSharp.Quotations
open System.Linq.Expressions

module Methods =

    /// We want to eliminate enum-types and constants like 1 or "a".
    /// But Constant value can be also another complex object like IQueryable.
    /// We don't want to evaluate those!
    let internal ``constant basic type`` (parentExpr:Expression) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Constant, (:? ConstantExpression as ce) 
    #if NET35 || NET45
            when parentExpr.Type.IsPrimitive -> 
    #else
            when parentExpr.Type.GetTypeInfo().IsPrimitive -> 
    #endif
                let getCorrectType (x:obj) = 
                    if (x :? IComparable) && x.GetType() = parentExpr.Type then Some (x :?> IComparable)
                    else None
                if ce.Value = null then None else
                match getCorrectType ce.Value with
                | None -> Expression.Lambda(parentExpr).Compile().DynamicInvoke(null) |> getCorrectType
                | x -> x
        | _ -> None
    

    /// Purpose of this is optimize away already known constant=constant style expressions.
    ///   7 > 8      -->   False
    /// "G" = "G"    -->   True
    let ``replace constant comparison`` (e:Expression) =
        match e with
        | (:? BinaryExpression as ce) -> 
            let (|Constant|_|) (e:Expression) = 
                match e.NodeType, e with 
                | ExpressionType.Constant, (:? ConstantExpression as ce) when (ce.Value :? IComparable) -> Some (ce.Value :?> IComparable)
                | ExpressionType.Convert, (:? UnaryExpression as ue) -> ``constant basic type`` ue ue.Operand
                | _ -> None
            let createbool b = Expression.Constant(b,  typeof<bool>) :> Expression
            match ce.Left, ce.Right with
            | Constant l, Constant r -> 
                match e.NodeType with
                | ExpressionType.Equal              -> createbool (l=r)
                | ExpressionType.LessThan           -> createbool (l<r)
                | ExpressionType.LessThanOrEqual    -> createbool (l<=r)
                | ExpressionType.GreaterThan        -> createbool (l>r)
                | ExpressionType.GreaterThanOrEqual -> createbool (l>=r)
                | ExpressionType.NotEqual           -> createbool (l<>r)
                | _ -> e
            | _ -> e
        | _ -> e


    /// Purpose of this is to replace non-used anonymous types:
    /// new AnonymousObject(Item1 = x, Item2 = "").Item1    -->   x
    let ``remove AnonymousType`` (e:Expression) =
        if e = Unchecked.defaultof<Expression> then e else 
        match e.NodeType, e with
        //FShap anonymous type:
        | ExpressionType.MemberAccess, ( :? MemberExpression as me)
            when me.Member.DeclaringType.Name.ToUpper().StartsWith("ANONYMOUSOBJECT") || me.Member.DeclaringType.Name.ToUpper().StartsWith("TUPLE") ->
                let memberIndex = 
                    if me.Member.Name.StartsWith("Item") && me.Member.Name.Length > 4 then
                        let ok, i = Int32.TryParse(me.Member.Name.Substring(4))
                        if ok then Some i else None
                    else None
                match memberIndex, me.Expression.NodeType, me.Expression, me.Member with 
                | Some idx, ExpressionType.New, (:? NewExpression as ne), (:? PropertyInfo as p) when ne <> null && p <> null -> 
                    if ne.Arguments.Count > idx - 1 && ne.Arguments.[idx-1].Type = p.PropertyType then 
                        ne.Arguments.[idx-1] // We found it!
                    else e
                | _ -> e
        //CSharp anonymous type:
        | ExpressionType.MemberAccess, ( :? MemberExpression as me)
            when me.Member.DeclaringType.Name.ToUpper().StartsWith("<>F__ANONYMOUSTYPE") || me.Member.DeclaringType.Name.ToUpper().StartsWith("TUPLE") ->
                match me.Expression.NodeType, me.Expression, me.Member with 
                | ExpressionType.New, (:? NewExpression as ne), (:? PropertyInfo as p) when ne.Arguments <> null && p <> null -> 
                        let selected = ne.Arguments |> Seq.tryPick(function
                            | :? MemberExpression as ame when ame.Member <> null && ame.Member.Name = me.Member.Name && ame.Type = p.PropertyType ->
                               Some(ame :> Expression)
                            |_ -> None)
                        match selected with 
                        | Some x -> x 
                        | None when ne.Members <> null -> 
                            let selected = ne.Members |> Seq.tryPick(function
                                | m when m.Name = me.Member.Name ->
                                    let idx = ne.Members.IndexOf(m)
                                    if ne.Arguments.Count > idx then
                                        match ne.Arguments.[idx] with
                                        | :? ParameterExpression as ape when ape.Type = p.PropertyType ->
                                            Some(ape :> Expression)
                                        | _ -> None
                                    else None
                                |_ -> None)
                            match selected with Some x -> x | None -> e
                        | None -> e
                | _ -> e
        | _ -> e


    /// if false then x else y -> y 
    let ``cut not used condition`` (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Conditional,        (:? ConditionalExpression as ce) -> 
            match ce.Test with // For now, only direct booleans conditions are optimized to select query:
            | :? ConstantExpression as c when c.Value = box(true) -> ce.IfTrue
            | :? ConstantExpression as c when c.Value = box(false) -> ce.IfFalse
            | _ -> e
        | _ -> e

    /// not(false) -> true 
    let ``not false is true``(e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Not, (:? UnaryExpression as ue) -> 
            match ue.Operand with
            | :? ConstantExpression as c when c.Value = box(false) -> Expression.Constant(true, typeof<bool>) :> Expression
            | :? ConstantExpression as c when c.Value = box(true) -> Expression.Constant(false, typeof<bool>) :> Expression
            | _ -> e
        | _ -> e



    // --------------- SOME BOOLEAN ALGEBRA ----------------------/
    // Idea from https://github.com/mavnn/Algebra.Boolean/blob/6b2099420ef605e3b3f818883db957154afa836a/Algebra.Boolean/Transforms.fs
    // But System.Linq.Expressions, not Microsoft.FSharp.Quotations

    // Reductions:
    // [associate; commute; distribute; gather; identity; annihilate; absorb; idempotence; complement; doubleNegation; deMorgan]

    let internal (|Value|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Constant, (:? ConstantExpression as ce) -> Some (ce.Value, ce.Type)
        | _ -> None

    let internal (|IfThenElse|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Conditional, (:? ConditionalExpression as ce) -> Some (ce.Test, ce.IfTrue, ce.IfFalse)
        | _ -> None

    let internal (|Not'|_|) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Not, (:? UnaryExpression as ue) -> Some(ue.Operand)
        | _ -> None

    let internal (|True'|_|) expr =
        match expr with
        | Value (o, t) when t = typeof<bool> && (o :?> bool) = true ->
            Some expr
        | _ -> None

    let internal (|False'|_|) expr =
        match expr with
        | Value (o, t) when t = typeof<bool> && (o :?> bool) = false ->
            Some expr
        | _ -> None

    let internal (|Or'|_|) (e:Expression) =
        match e.NodeType, e with
        | _, IfThenElse (left, True' _, right) ->
            Some (left, right)
        | ExpressionType.OrElse, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
        //| ExpressionType.Or, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
        | _ -> None

    let internal (|And'|_|) (e:Expression) =
        match e.NodeType, e with
        | _, IfThenElse (left, right, False' _) ->
            Some (left, right)
        | ExpressionType.AndAlso, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
        //| ExpressionType.And, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
        | _ -> None

    /// Not in use, would cause looping...
    let associate = function
        | Or' (Or' (l, r), r') -> Expression.OrElse(Expression.OrElse(l, r), r') :> Expression
        | Or' (l, Or' (l', r)) -> Expression.OrElse(l, Expression.OrElse(l', r)) :> Expression
        | And' (And' (l, r), r') -> Expression.AndAlso(Expression.AndAlso(l, r), r') :> Expression
        | And' (l, And' (l', r)) -> Expression.AndAlso(l, Expression.AndAlso(l', r)) :> Expression
        | noHit -> noHit

    // We commute to AndAlso and OrElse, if not already in that format
    let commute = function
        | Or' (left, right) as comex when comex.NodeType <> ExpressionType.OrElse -> Expression.OrElse(right, left) :> Expression
        | And' (left, right) as comex when comex.NodeType <> ExpressionType.AndAlso -> Expression.AndAlso(right, left) :> Expression
        | noHit -> noHit

    /// Not in use, would cause looping...
    let distribute = function
        | And' (p, Or' (p', p'')) -> Expression.OrElse(Expression.AndAlso(p, p'), Expression.AndAlso(p, p'')) :> Expression
        | Or' (p, And' (p', p'')) -> Expression.AndAlso(Expression.OrElse(p, p'), Expression.OrElse(p, p'')) :> Expression
        | noHit -> noHit

    let gather = function
        | And' (Or'(p, p'), Or'(p'', p''')) when p = p'' -> Expression.OrElse(p, Expression.AndAlso(p', p''')) :> Expression
        | Or' (And'(p, p'), And'(p'', p''')) when p = p'' -> Expression.AndAlso(p, Expression.OrElse(p', p''')) :> Expression
        | noHit -> noHit

    let identity = function
        | And' (True' _, p)
        | And' (p, True' _)
        | Or' (False' _, p) 
        | Or' (p, False' _)
            -> p
        | noHit -> noHit

    let annihilate = function
        | And' (False' f, _)
        | And' (_, False' f) -> f
        | Or' (True' t, _) 
        | Or' (_, True' t) -> t
        | noHit -> noHit

    let absorb = function
        | And' (p, Or' (p', _)) 
        | And' (p, Or' (_, p')) 
        | And' (Or' (p', _), p)
        | And' (Or' (_, p'), p)
        | Or' (p, And' (p', _))
        | Or' (p, And' (_, p'))
        | Or' (And' (p', _), p)
        | Or' (And' (_, p'), p) when p = p' -> p
        | noHit -> noHit

    let idempotence = function
        | And' (p, p') when p = p' -> p
        | Or' (p, p')  when p = p' -> p
        | noHit -> noHit

    let complement = function
        | And' (p, Not' p')
        | And' (Not' p, p') when p = p' -> Expression.Constant(false, typeof<bool>) :> Expression
        | Or' (p, Not' p')
        | Or' (Not' p, p') when p = p' -> Expression.Constant(true, typeof<bool>) :> Expression
        | noHit -> noHit

    let doubleNegation = function
        | Not' (Not' p) -> p
        | noHit -> noHit

    let deMorgan = function
        | Or' (Not' p, Not' p') -> Expression.Not(Expression.AndAlso(p, p')) :> Expression
        | And' (Not' p, Not' p') -> Expression.Not(Expression.OrElse(p, p')) :> Expression
        | noHit -> noHit

    // ------------------------------------- //

    /// Balance tree that is too much weighted to other side.
    /// The real advantage is not-so-nested-stack
    let balancetree = function
        | Or' (p1,  Or' (p2,  Or' (p3,  Or' (p4,  Or' (p5, Or' (p6, Or' (p7, p8))))))) 
        | Or' (Or' (Or' (Or' (Or' (Or' (Or' (p1, p2), p3), p4), p5), p6), p7), p8) 
            -> Expression.OrElse(Expression.OrElse(Expression.OrElse(p1, p2), Expression.OrElse(p3, p4)), Expression.OrElse(Expression.OrElse(p5, p6), Expression.OrElse(p7, p8))) :> Expression
        | And' (p1,  And' (p2,  And' (p3,  And' (p4,  And' (p5, And' (p6, And' (p7, p8))))))) 
        | And' (And' (And' (And' (And' (And' (And' (p1, p2), p3), p4), p5), p6), p7), p8) 
            -> Expression.AndAlso(Expression.AndAlso(Expression.AndAlso(p1, p2), Expression.AndAlso(p3, p4)), Expression.AndAlso(Expression.AndAlso(p5, p6), Expression.AndAlso(p7, p8))) :> Expression
        | noHit -> noHit

    // ------------------------------------- //
    /// Evaluating constants to not mess with our expressions:
    let ``evaluate constants`` (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.MemberAccess, ( :? MemberExpression as me) 
            when me <> null && me.Expression<>null -> 
                match ``constant basic type`` me me.Expression with
                | Some x -> Expression.Constant(x, me.Type) :> Expression
                | _ -> e
        | ExpressionType.MemberAccess, ( :? MemberExpression as me) 
            when me <> null && me.Expression=null && 
                    (me.Member.DeclaringType.Name.ToUpper().StartsWith("FSI_") 
                     || me.Member.DeclaringType.Name.ToUpper().StartsWith("<>C__DISPLAYCLASS") ) -> 
                match me.Member with 
                | :? PropertyInfo as p when p.GetType().FullName.StartsWith("System") -> 
                        Expression.Constant(Expression.Lambda(me).Compile().DynamicInvoke(null), me.Type) :> Expression
                | _ -> e
        | _ -> e

    // ------------------------------------- //
    /// Evaluate simple math between two constants.
    ///  9  *  3     -->    27
    /// "G" + "G"    -->   "GG"
    let  ``evaluate basic constant math`` (e:Expression) =
        match e with
        | (:? BinaryExpression as ce) -> 
            if ce.Left.Type <> ce.Right.Type then e
            else
            match ce.Left.NodeType, ce.Right.NodeType, ce.Left, ce.Right with
            | ExpressionType.Constant, ExpressionType.Constant, (:? ConstantExpression as le), (:? ConstantExpression as ri) ->
                // F# doesn't support macros so this code is a bit copy-and-paste, but it should be trivial. 
                match e.NodeType with
                | ExpressionType.Add ->
                    match le.Value, ri.Value with
                    | (:? string  as lstr), (:? string  as rstr) when le.Type = typeof<string>  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? decimal as lstr), (:? decimal as rstr) when le.Type = typeof<decimal> -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when le.Type = typeof<float32> -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when le.Type = typeof<double>  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when le.Type = typeof<float>   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when le.Type = typeof<Int32>   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when le.Type = typeof<Int64>   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when le.Type = typeof<UInt32>  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when le.Type = typeof<UInt64>  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when le.Type = typeof<Int16>   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when le.Type = typeof<UInt16>  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when le.Type = typeof<int8>    -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when le.Type = typeof<uint8>   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Subtract -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when le.Type = typeof<decimal> -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when le.Type = typeof<float32> -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when le.Type = typeof<double>  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when le.Type = typeof<float>   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when le.Type = typeof<Int32>   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when le.Type = typeof<Int64>   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when le.Type = typeof<UInt32>  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when le.Type = typeof<UInt64>  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when le.Type = typeof<Int16>   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when le.Type = typeof<UInt16>  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when le.Type = typeof<int8>    -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when le.Type = typeof<uint8>   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Multiply -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when le.Type = typeof<decimal> -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when le.Type = typeof<float32> -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when le.Type = typeof<double>  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when le.Type = typeof<float>   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when le.Type = typeof<Int32>   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when le.Type = typeof<Int64>   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when le.Type = typeof<UInt32>  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when le.Type = typeof<UInt64>  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when le.Type = typeof<Int16>   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when le.Type = typeof<UInt16>  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when le.Type = typeof<int8>    -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when le.Type = typeof<uint8>   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Divide -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when le.Type = typeof<decimal> -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when le.Type = typeof<float32> -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when le.Type = typeof<double>  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when le.Type = typeof<float>   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when le.Type = typeof<Int32>   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when le.Type = typeof<Int64>   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when le.Type = typeof<UInt32>  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when le.Type = typeof<UInt64>  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when le.Type = typeof<Int16>   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when le.Type = typeof<UInt16>  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when le.Type = typeof<int8>    -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when le.Type = typeof<uint8>   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Modulo -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when le.Type = typeof<decimal> -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when le.Type = typeof<float32> -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when le.Type = typeof<double>  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when le.Type = typeof<float>   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when le.Type = typeof<Int32>   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when le.Type = typeof<Int64>   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when le.Type = typeof<UInt32>  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when le.Type = typeof<UInt64>  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when le.Type = typeof<Int16>   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when le.Type = typeof<UInt16>  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when le.Type = typeof<int8>    -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when le.Type = typeof<uint8>   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | _ -> e
                | _ -> e
            | _ -> e
        | _ -> e

// ------------------------------------- //
/// Used optimization methods
let mutable reductionMethods = [
     Methods.``evaluate constants``;  Methods.``evaluate basic constant math``
     Methods.``replace constant comparison``; Methods.``remove AnonymousType``; 
     Methods.``cut not used condition``; Methods.``not false is true``;
     (*Methods.associate;*) Methods.commute; (*Methods.distribute;*) Methods.gather; Methods.identity; 
     Methods.annihilate; Methods.absorb; Methods.idempotence; Methods.complement; Methods.doubleNegation; 
     Methods.deMorgan; Methods.balancetree]

/// Does reductions just for a current node.
let rec doReduction (exp:Expression) =
    if exp = Unchecked.defaultof<Expression> then exp else 
    let opt = reductionMethods |> Seq.fold(fun acc f -> f(acc)) exp
    match opt = exp with
    | true -> exp
    | false -> doReduction opt

// ------------------------------------- //

// Expression tree visitor: go through the whole expression tree.

// .NET has already this System.Linq.Expressions.Expressionvisitor
// Too bad this was so simple and faster than what it would have taken to get to know that 700 rows of source code!
let rec internal visit' (exp:Expression): Expression =
    //bottom up:
    if exp = null then null else
    let e1 = visitchilds exp
    let e2 = doReduction e1
    e2

and internal visitchilds (e:Expression): Expression =

    if e = null then null else
    match e with
    | (:? ConstantExpression as e)    -> 
       let v = ``WhereSelectEnumerableIterator visitor`` e
       upcast v
    | (:? ParameterExpression as e)   -> upcast e
    | (:? UnaryExpression as e) -> 
        let visit'ed = visit' e.Operand
        if visit'ed=e.Operand then upcast e else upcast Expression.MakeUnary(e.NodeType,visit'ed,e.Type,e.Method) 
    | (:? BinaryExpression as e)      -> 
        if e.NodeType = ExpressionType.Coalesce && e.Conversion <> null then
            let v1, v2, v3 = visit' e.Left, visit' e.Right, visit' e.Conversion
            if v1=e.Left && v2=e.Right && v3=(e.Conversion:>Expression) then upcast e else upcast Expression.Coalesce(v1, v2, v3 :?> LambdaExpression)
        else
            let v1, v2 = visit' e.Left, visit' e.Right
            if v1=e.Left && v2=e.Right then upcast e else upcast Expression.MakeBinary(e.NodeType,v1,v2,e.IsLiftedToNull, e.Method)
    | (:? MemberExpression as e)      -> 
        let v = visit' e.Expression
        if v=e.Expression then upcast e else upcast Expression.MakeMemberAccess(v, e.Member)
    | (:? MethodCallExpression as e)  -> 
        let obje = visit' e.Object
        let args = e.Arguments |> Seq.toArray 
        let visited = args |> Array.map(fun a -> visit' a)
        if e.Object = obje && args = visited then upcast e else
        upcast Expression.Call(obje, e.Method, visited)
    | (:? LambdaExpression as e)      -> 
        let b = visit' e.Body 
        if b=e.Body then upcast e else upcast Expression.Lambda(e.Type, b, e.Parameters)
    | (:? TypeBinaryExpression as e)  -> 
        let v = visit'(e.Expression)
        if v=e.Expression then upcast e else upcast Expression.TypeIs(v, e.TypeOperand)
    | (:? ConditionalExpression as e) -> 
        let v1, v2, v3 = visit' e.Test, visit' e.IfTrue, visit' e.IfFalse
        if v1=e.Test && v2=e.IfTrue && v3=e.IfFalse then upcast e else upcast Expression.Condition(v1, v2, v3)
    | (:? NewExpression as e) when e.Members = null -> upcast Expression.New(e.Constructor, e.Arguments |> Seq.map(fun a -> visit' a))
    | (:? NewExpression as e) when e.Members <> null -> upcast Expression.New(e.Constructor, e.Arguments |> Seq.map(fun a -> visit' a), e.Members)
    | (:? NewArrayExpression as e) when e.NodeType = ExpressionType.NewArrayBounds ->
                                         upcast Expression.NewArrayBounds(e.Type.GetElementType(), e.Expressions |> Seq.map(fun e -> visit' e))
    | (:? NewArrayExpression as e)    -> upcast Expression.NewArrayInit(e.Type.GetElementType(), e.Expressions |> Seq.map(fun e -> visit' e))
    | (:? InvocationExpression as e)  -> upcast Expression.Invoke(visit' e.Expression, e.Arguments |> Seq.map(fun a -> visit' a))
    | (:? MemberInitExpression as e)  -> upcast Expression.MemberInit( (visit' e.NewExpression) :?> NewExpression , e.Bindings) //probably shoud visit' also bindings
    | (:? ListInitExpression as e)    -> upcast Expression.ListInit( (visit' e.NewExpression) :?> NewExpression, e.Initializers) //probably shoud visit' also initialixers
    | _ -> if (int e.NodeType = 52) then e // Expression.Extension
           else failwith ("encountered unknown LINQ expression: " + e.NodeType.ToString() + " " + e.ToString())

// Look also inside a LINQ-wrapper
// https://referencesource.microsoft.com/#System.Core/System/Linq/Enumerable.cs,8bf16962931637d3,references
and internal ``WhereSelectEnumerableIterator visitor`` (ce:ConstantExpression) : ConstantExpression =
    if ce.Value = null || ce.Type = null || (not (ce.Type.FullName.StartsWith "System.Linq")) then ce
    else
    let enuProp = ce.Type.GetProperty("Enumerable", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    if enuProp = null then ce
    else
    let enu = enuProp.GetValue(ce.Value, null)
    if enu = null then ce
    else
    let srcProp = enu.GetType().GetField("source", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    if srcProp = null then ce
    else 
    let src = srcProp.GetValue enu
    if src = null then ce
    else
    let exprItm = src.GetType().GetField("expression", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    let expr = exprItm.GetValue src
    if expr = null then ce
    else
    let exp = expr :?> Expression
    let opt = visit' exp
    if opt <> exp then
        exprItm.SetValue(src, (box opt))
    ce

/// Expression tree visitor: go through the whole expression tree.
let visit exp =
    visit' exp

/// Expression tree visitor: go through the whole expression tree.
/// Catches the exceptions.
let tryVisit exp =
    try visit' exp
    with _ -> exp