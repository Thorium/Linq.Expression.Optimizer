/// This is just a light-weight expression optimizer.
/// It won't do any heavy stuff...
#if !INTERACTIVE
module ExpressionOptimizer
#endif

open System.Linq.Expressions
open System
open System.Reflection


open Microsoft.FSharp.Quotations
open System.Linq.Expressions

module Methods =

    let internal propertyMatch (p:Expression) (p2:Expression) =
        if p.NodeType <> p2.NodeType then false
        else
        match p.NodeType, p, p2 with
        | ExpressionType.MemberAccess, (:? MemberExpression as pe1), (:? MemberExpression as pe2) -> 
            (pe1.Member = pe2.Member) && (pe1.Expression = pe2.Expression) && pe1.ToString() = pe2.ToString()
        | ExpressionType.Constant, (:? ConstantExpression as ce1), (:? ConstantExpression as ce2) -> 
            ce1.Value = ce2.Value
        | _ -> false
        

    /// We want to eliminate enum-types and constants like 1 or "a".
    /// But the constant value can also be another complex object, such as IQueryable.
    /// We don't want to evaluate those!
    let inline internal ``constant basic type`` (parentExpr:Expression) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Constant, (:? ConstantExpression as ce) 
    #if NET35 || NET45
            when parentExpr.Type.IsPrimitive -> 
    #else
            when parentExpr.Type.GetTypeInfo().IsPrimitive -> 
    #endif
                let getCorrectType (x:obj) = 
                    if (x :? IComparable) && Type.(=)(x.GetType(), parentExpr.Type) then Some (x :?> IComparable)
                    else None
                if isNull ce.Value then None else
                match getCorrectType ce.Value with
                | None -> 
                     //Expression.Lambda(parentExpr).Compile().DynamicInvoke(null) |> getCorrectType

                     let myVal = 
                         match parentExpr with
                         | :? MemberExpression as mainNode ->
                             match mainNode.Member with
                             | :? FieldInfo as fieldInfo when not(isNull(fieldInfo)) ->
                                fieldInfo.GetValue ce.Value
                             | :? PropertyInfo as propInfo when not(isNull(propInfo)) ->
                                propInfo.GetValue(ce.Value, null)
                             | _ -> ce.Value
                         | _ -> ce.Value
                     myVal |> getCorrectType
                | x -> x
        | _ -> None
    

    /// The purpose of this is to optimize away already known constant=constant style expressions.
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
            match ce.Left, ce.Right with
            | Constant l, Constant r -> 
                let createbool b = Expression.Constant(b,  typeof<bool>) :> Expression
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
            when me.Member.DeclaringType.Name.ToUpper().StartsWith("ANONYMOUSOBJECT") || me.Member.DeclaringType.Name.ToUpper().StartsWith "TUPLE" ->
                let memberIndex = 
                    if me.Member.Name.StartsWith("Item") && me.Member.Name.Length > 4 then
#if NETSTANDARD21
                        let ok, i = Int32.TryParse(me.Member.Name.AsSpan 4)
#else
                        let ok, i = Int32.TryParse(me.Member.Name.Substring 4)
#endif
                        if ok then ValueSome i else ValueNone
                    else ValueNone
                match memberIndex, me.Expression.NodeType, me.Expression, me.Member with 
                | ValueSome idx, ExpressionType.New, (:? NewExpression as ne), (:? PropertyInfo as p) when not(isNull ne || isNull p) -> 
                    if ne.Arguments.Count > idx - 1 && Type.(=) (ne.Arguments.[idx-1].Type, p.PropertyType) then 
                        ne.Arguments.[idx-1] // We found it!
                    else e
                | _ -> e
        //CSharp anonymous type:
        | ExpressionType.MemberAccess, ( :? MemberExpression as me)
            when me.Member.DeclaringType.Name.ToUpper().StartsWith("<>F__ANONYMOUSTYPE") || me.Member.DeclaringType.Name.ToUpper().StartsWith "TUPLE" ->
                match me.Expression.NodeType, me.Expression, me.Member with 
                | ExpressionType.New, (:? NewExpression as ne), (:? PropertyInfo as p) when not(isNull ne.Arguments || isNull p) -> 
                        let selected = ne.Arguments |> Seq.tryPick(function
                            | :? MemberExpression as ame when (not(isNull ame.Member)) && ame.Member.Name = me.Member.Name && Type.(=) (ame.Type, p.PropertyType) ->
                               Some(ame :> Expression)
                            |_ -> None)
                        match selected with 
                        | Some x -> x 
                        | None when not(isNull ne.Members) -> 
                            let selected = ne.Members |> Seq.tryPick(function
                                | m when m.Name = me.Member.Name ->
                                    let idx = ne.Members.IndexOf m
                                    if ne.Arguments.Count > idx then
                                        match ne.Arguments.[idx] with
                                        | :? ParameterExpression as ape when Type.(=) (ape.Type, p.PropertyType) ->
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
            match ce.Test.NodeType, ce.Test with // For now, only direct booleans conditions are optimized to select query:
            | ExpressionType.Constant, (:? ConstantExpression as c) when c.Value = box(true) -> ce.IfTrue
            | ExpressionType.Constant, (:? ConstantExpression as c) when c.Value = box(false) -> ce.IfFalse
            | _ -> e
        | _ -> e

    /// not(false) -> true 
    let ``not false is true``(e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Not, (:? UnaryExpression as ue) -> 
            match ue.Operand.NodeType, ue.Operand with
            | ExpressionType.Constant, (:? ConstantExpression as c) when c.Value = box(false) -> Expression.Constant(true, typeof<bool>) :> Expression
            | ExpressionType.Constant, (:? ConstantExpression as c) when c.Value = box(true) -> Expression.Constant(false, typeof<bool>) :> Expression
            | _ -> e
        | _ -> e



    // --------------- SOME BOOLEAN ALGEBRA ----------------------/
    // Idea from https://github.com/mavnn/Algebra.Boolean/blob/6b2099420ef605e3b3f818883db957154afa836a/Algebra.Boolean/Transforms.fs
    // But System.Linq.Expressions, not Microsoft.FSharp.Quotations

    // Reductions:
    // [associate; commute; distribute; gather; identity; annihilate; absorb; idempotence; complement; doubleNegation; deMorgan]
    [<return: Struct>]
    let inline internal (|ValueBool|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Constant, (:? ConstantExpression as ce) when Type.(=) (ce.Type, typeof<bool>) -> ValueSome (ce.Value :?> bool)
        | ExpressionType.MemberAccess, (:? MemberExpression as me) when (me.Expression :? ConstantExpression) && Type.(=) ((me.Expression :?> ConstantExpression).Type, typeof<bool>) -> 
            let ceVal = (me.Expression :?> ConstantExpression).Value
            let myVal = 
                match me.Member with
                | :? FieldInfo as fieldInfo when not(isNull(fieldInfo)) ->
                    fieldInfo.GetValue ceVal
                | :? PropertyInfo as propInfo when not(isNull(propInfo)) ->
                    propInfo.GetValue(ceVal, null)
                | _ -> ceVal
            ValueSome (myVal :?> bool)
        | _ -> ValueNone

    let inline internal (|ComparisonExpression|_|) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.LessThan,           (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | ExpressionType.LessThanOrEqual,    (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | ExpressionType.GreaterThan,        (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | ExpressionType.GreaterThanOrEqual, (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | ExpressionType.Equal,              (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | ExpressionType.NotEqual,           (:? BinaryExpression as e) -> Some (e.NodeType, e.Left, e.Right)
        | _ -> None

    let inline internal (|IfThenElse|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Conditional, (:? ConditionalExpression as ce) -> Some (ce.Test, ce.IfTrue, ce.IfFalse)
        | _ -> None

    let inline internal (|Not'|_|) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Not, (:? UnaryExpression as ue) -> Some(ue.Operand)
        | _ -> None

    [<return: Struct>]
    let inline internal (|True'|_|) expr =
        match expr with
        | ValueBool o when o ->
            ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let inline internal (|False'|_|) expr =
        match expr with
        | ValueBool o when not o ->
            ValueSome()
        | _ -> ValueNone

    let inline internal (|Or'|_|) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.OrElse, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
        | ExpressionType.Or, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
        | ExpressionType.Conditional, IfThenElse (left, True' _, right) ->
            Some (left, right)
        //| ExpressionType.Or, ( :? BinaryExpression as be) -> Some(be.Left,be.Right)
        | _ -> None

    let inline internal (|And'|_|) (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.AndAlso, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
        | ExpressionType.And, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
        | ExpressionType.Conditional, IfThenElse (left, right, False' _) ->
            Some (left, right)
        //| ExpressionType.And, ( :? BinaryExpression as be)  -> Some(be.Left,be.Right)
        | _ -> None

    /// Not in use, would cause looping...
    let associate = function
        | Or' (Or' (l, r), r') -> Expression.OrElse(l, Expression.OrElse(r, r')) :> Expression
        | Or' (l, Or' (l', r)) -> Expression.OrElse(Expression.OrElse(l, l'), r) :> Expression
        | And' (And' (l, r), r') -> Expression.AndAlso(l, Expression.AndAlso(r, r')) :> Expression
        | And' (l, And' (l', r)) -> Expression.AndAlso(Expression.AndAlso(l, l'), r) :> Expression
        | noHit -> noHit

    // We commute to AndAlso and OrElse, if not already in that format
    let commute = function
        | Or' (left, right) as comex when comex.NodeType <> ExpressionType.OrElse -> Expression.OrElse(left, right) :> Expression
        | And' (left, right) as comex when comex.NodeType <> ExpressionType.AndAlso -> Expression.AndAlso(left, right) :> Expression
        | noHit -> noHit

    /// Not in use, would cause looping...
    let commute2 = function
        | Or' (left, right) -> Expression.OrElse(right, left) :> Expression
        | And' (left, right) -> Expression.AndAlso(right, left) :> Expression
        | noHit -> noHit

    /// Not in use, would cause looping...
    /// This is opposite of gather
    let distribute = fun e ->
        match e with
        | And' (p, Or' (p1, p2)) -> Expression.OrElse(Expression.AndAlso(p, p1), Expression.AndAlso(p, p2)) :> Expression
        | Or' (p, And' (p1, p2)) -> Expression.AndAlso(Expression.OrElse(p, p1), Expression.OrElse(p, p2)) :> Expression
        | And' (Or' (p1, p2), p) -> Expression.OrElse(Expression.AndAlso(p1, p), Expression.AndAlso(p2, p)) :> Expression
        | Or' (And' (p1, p2), p) -> Expression.AndAlso(Expression.OrElse(p1, p), Expression.OrElse(p2, p)) :> Expression
        | noHit -> noHit

    let gather = function
        | And' (Or'(p, p1), Or'(p2, p3)) when p = p2 || propertyMatch p p2 -> Expression.OrElse(p, Expression.AndAlso(p1, p3)) :> Expression
        | Or' (And'(p, p1), And'(p2, p3)) when p = p2 || propertyMatch p p2 -> Expression.AndAlso(p, Expression.OrElse(p1, p3)) :> Expression
        | And' (Or'(p, p1), Or'(p3, p2)) when p = p2 || propertyMatch p p2 -> Expression.OrElse(p, Expression.AndAlso(p1, p3)) :> Expression
        | Or' (And'(p, p1), And'(p3, p2)) when p = p2 || propertyMatch p p2 -> Expression.AndAlso(p, Expression.OrElse(p1, p3)) :> Expression
        | noHit -> noHit

    let identity = function
        | And' (True' _, p)
        | And' (p, True' _)
        | Or' (False' _, p) 
        | Or' (p, False' _)
            -> p
        | noHit -> noHit

    let annihilate = function
        | And' (False' _, _)
        | And' (_, False' _) -> Expression.Constant(false, typeof<bool>) :> Expression
        | Or' (True' _, _) 
        | Or' (_, True' _) -> Expression.Constant(true, typeof<bool>) :> Expression
        | noHit -> noHit

    let absorb = function
        | And' (p, Or' (p1, _)) 
        | And' (p, Or' (_, p1)) 
        | And' (Or' (p1, _), p)
        | And' (Or' (_, p1), p)
        | Or' (p, And' (p1, _))
        | Or' (p, And' (_, p1))
        | Or' (And' (p1, _), p)
        | Or' (And' (_, p1), p) when p = p1 || propertyMatch p p1 -> p
        | noHit -> noHit

    let idempotence = function
        | And' (p, p1) when p = p1 || propertyMatch p p1 -> p
        | Or' (p, p1)  when p = p1 || propertyMatch p p1 -> p
        | Not'(And' (p, p1)) when p = p1 || propertyMatch p p1 -> Expression.Not(p)
        | Not'(Or' (p, p1))  when p = p1 || propertyMatch p p1 -> Expression.Not(p)
        | noHit -> noHit

    let complement = function
        | And' (p, Not' p1)
        | And' (Not' p, p1) when p = p1 || propertyMatch p p1 -> Expression.Constant(false, typeof<bool>) :> Expression
        | Or' (p, Not' p1)
        | Or' (Not' p, p1) when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
        | noHit -> noHit

    let commute_absorb = fun exp ->
        match exp with
        | And' (innercontent) ->
            match innercontent with
            | (Or'(_,Or'(p,_)), p1)
            | (Or'(Or'(p,_), _), p1)
            | (Or'(_,Or'(_,p)), p1)
            | (Or'(Or'(_,p), _), p1)
            | (p1, Or'(Or'(p,_), _))
            | (p1, Or'(_,Or'(p,_))) 
            | (p1, Or'(Or'(_,p), _))
            | (p1, Or'(_,Or'(_,p))) 
                when p = p1 || propertyMatch p p1 -> p

            | (p, And'(p2,(Or'(p1,_)))) 
            | (p, And'(p2,(Or'(_,p1)))) 
            | (And'(p2,(Or'(p1,_))), p) 
            | (And'(p2,(Or'(_,p1))), p) 
            | (p, And'((Or'(p1,_)),p2)) 
            | (p, And'((Or'(_,p1)),p2)) 
            | (And'((Or'(p1,_)),p2), p) 
            | (And'((Or'(_,p1)),p2), p) 
                when p = p1 || propertyMatch p p1  -> Expression.AndAlso(p,p2)
            | _ -> exp
        | Or' (innercontent) ->
            match innercontent with
            | (p, Or'(p2, And' (p1, _)))
            | (p, Or'(p2, And' (_, p1)))
            | (p, Or'(And' (p1, _), p2))
            | (p, Or'(And' (_, p1), p2)) 
            | (Or'(And' (p1, _), p2), p)
            | (Or'(And' (_, p1), p2), p) when p = p1 || propertyMatch p p1 -> Expression.OrElse(p,p2)
            | (Or'(p2, And' (p1, _)), p)
            | (Or'(p2, And' (_, p1)), p) when p = p1 || propertyMatch p p1 -> Expression.OrElse(p2,p)
            | (Or'(p, p2), And' (p1, _))
            | (Or'(p, p2), And' (_, p1))
            | (And' (p1, _), Or'(p, p2))
            | (And' (p1, _), Or'(p2, p))
            | (And' (_, p1), Or'(p, p2))
            | (And' (_, p1), Or'(p2, p)) when p = p1 || propertyMatch p p1 -> Expression.OrElse(p,p2)
            | (Or'(p2, p), And' (p1, _))
            | (Or'(p2, p), And' (_, p1)) when p = p1 || propertyMatch p p1 -> Expression.OrElse(p2,p)
            | _ -> exp
        | noHit -> noHit

    // This is to fix some part of pure distribute being disabled
    let distribute_complement = fun exp ->


        // duplicate looking active pattern "when" guards are needed for some weird F# compiler reason being slow & eating stack & memory if they are not there
        match exp with
        | And' (innercontent) ->
            // All the following are decorated with And'( ... )
            match innercontent with
            | (p, Or' (Not' p1, p2))  
            | (p, Or' (p2, Not' p1))  
            | (Or' (Not' p1, p2), p)  
                when p = p1 || propertyMatch p p1 -> Expression.AndAlso(p, p2) :> Expression
            | (Or' (p2, Not' p1), p)  
                when p = p1 || propertyMatch p p1 -> Expression.AndAlso(p2, p) :> Expression
            | _ -> exp
        | Or' (innercontent) ->
            // All the following are decorated with Or'( ... )
            match innercontent with

            | (p, And' (Not' p1, p2))  
            | (p, And' (p2, Not' p1))  
            | (And' (Not' p1, p2), p)  
                when p = p1 || propertyMatch p p1 -> Expression.OrElse(p, p2) :> Expression
            | (And' (p2, Not' p1), p)  
                when p = p1 || propertyMatch p p1 -> Expression.OrElse(p2, p) :> Expression

            | (Or'(p2, Not' p), And'(p1, Not' p4))
            | (Or'(Not' p, p2), And'(p1, Not' p4))
                when ((p = p1 && p2 = p4) || (propertyMatch p p1 && propertyMatch p2 p4)) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(Not' p, p2), And'(Not' p4, p1))
            | (Or'(Not' p, p2), And'(Not' p4, p1))
                when ((p = p1 && p2 = p4) || (propertyMatch p p1 && propertyMatch p2 p4)) -> Expression.Constant(true, typeof<bool>) :> Expression

            | (Not'(Or'(p, p2)), And'(p1, Not' p3))
            | (Not'(Or'(p2, p)), And'(p1, Not' p3)) when ((p = p1 && p2 = p3) || (propertyMatch p p1 && propertyMatch p2 p3)) -> Expression.Not(p2) :> Expression
            | (Not'(Or'(p, p2)), And'(Not' p3, p1))
            | (Not'(Or'(p, p2)), And'(Not' p3, p1)) when ((p = p1 && p2 = p3) || (propertyMatch p p1 && propertyMatch p2 p3)) -> Expression.Not(p2) :> Expression
            | (And'(p1, Not' p3), Not'(Or'(p, p2)))
            | (And'(p1, Not' p3), Not'(Or'(p2, p))) when ((p = p1 && p2 = p3) || (propertyMatch p p1 && propertyMatch p2 p3)) -> Expression.Not(p2) :> Expression
            | (And'(Not' p3, p1), Not'(Or'(p, p2)))
            | (And'(Not' p3, p1), Not'(Or'(p, p2))) when ((p = p1 && p2 = p3) || (propertyMatch p p1 && propertyMatch p2 p3)) -> Expression.Not(p2) :> Expression

            | (Or'(p, _), Or'(Not' p1, _)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, p), Or'(Not' p1, _)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(p, _), Or'(_, Not' p1)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, p), Or'(_, Not' p1)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(Not' p1, _), Or'(p, _)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(Not' p1, _), Or'(_, p)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, Not' p1), Or'(p, _)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, Not' p1), Or'(_, p)) when (p = p1 || propertyMatch p p1) -> Expression.Constant(true, typeof<bool>) :> Expression

            //// Eliminate negations of p towards Disjunctive Normal Form (after deMorgan already applied)

            | (Or'(Not' p3, p), And'(p4, Not' p1)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(Expression.OrElse(Expression.Not(p3),p4), p) :> Expression
            | (Or'(p, Not' p3), And'(p4, Not' p1)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p, Expression.OrElse(Expression.Not(p3),p4)) :> Expression
            | (Or'(Not' p3, p), And'(Not' p1, p4)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(Expression.OrElse(Expression.Not(p3),p4), p) :> Expression
            | (Or'(p, Not' p3), And'(Not' p1, p4)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p, Expression.OrElse(Expression.Not(p3),p4)) :> Expression
            | (And'(p4, Not' p1), Or'(Not' p3, p)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p4, Expression.OrElse(p,Expression.Not(p3))) :> Expression
            | (And'(p4, Not' p1), Or'(p, Not' p3)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p4, Expression.OrElse(p,Expression.Not(p3))) :> Expression
            | (And'(Not' p1, p4), Or'(Not' p3, p)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p, Expression.OrElse(p4, Expression.Not(p3))) :> Expression
            | (And'(Not' p1, p4), Or'(p, Not' p3)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p, Expression.OrElse(p4, Expression.Not(p3))) :> Expression
            

            | (Not'(Or'(p, p3)), And'(p4, Not' p1))  
            | (Not'(Or'(p, p3)), And'(Not' p1, p4)) when (p = p1 || propertyMatch p p1) -> Expression.AndAlso(Expression.Not(p), Expression.OrElse(Expression.Not p3, p4)) :> Expression
            | (And'(Not' p1, p4), Not'(Or'(p3, p))) 
            | (And'(Not' p1, p4), Not'(Or'(p, p3))) when (p = p1 || propertyMatch p p1) -> Expression.AndAlso(Expression.Not(p), Expression.OrElse(p4, Expression.Not p3)) :> Expression
            | (Not'(Or'(p3, p)), And'(Not' p1, p4))  
            | (Not'(Or'(p3, p)), And'(p4, Not' p1)) when (p = p1 || propertyMatch p p1) -> Expression.AndAlso(Expression.OrElse(Expression.Not p3, p4), Expression.Not(p)) :> Expression
            | (And'(p4, Not' p1), Not'(Or'(p3, p)))  
            | (And'(p4, Not' p1), Not'(Or'(p, p3))) when (p = p1 || propertyMatch p p1) -> Expression.AndAlso(Expression.OrElse(p4, Expression.Not p3), Expression.Not(p)) :> Expression

            | (Or'(p, p2), Not'(Or'(p3, p1))) 
            | (Or'(p, p2), Not'(Or'(p1, p3))) 
            | (Not'(Or'(p1, p3)), Or'(p, p2)) 
            | (Not'(Or'(p1, p3)), Or'(p2, p)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p, Expression.OrElse(p2, Expression.Not p3))
            | (Or'(p2, p), Not'(Or'(p3, p1))) 
            | (Or'(p2, p), Not'(Or'(p1, p3))) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(p2, Expression.OrElse(p, Expression.Not p3))
            | (Not'(Or'(p3, p1)), Or'(p, p2)) 
            | (Not'(Or'(p3, p1)), Or'(p2, p)) when (p = p1 || propertyMatch p p1) -> Expression.OrElse(Expression.Not p3, Expression.OrElse(p, p2))

            | _ -> exp
            
        | noHit -> noHit


    // This is to fix some part of pure associate being disabled
    let associate_complement = fun exp ->
        match exp with
        | And' (innercontent) ->
            // All the following are decorated with And'( ... )
            match innercontent with
            |(Not' (And' (p, p2)), p1) 
            |(Not' (And' (p2, p)), p1) 
            |(p1, Not' (And' (p, p2))) 
            |(p1, Not' (And' (p2, p))) 
                when p = p1 || propertyMatch p p1 -> Expression.AndAlso(p, Expression.Not p2) :> Expression
            |(And' (p, _), Not' p1)
            |(And' (Not' p, _), p1)
            |(Not' p1, And' (p, _))
            |(p1, And' (Not' p, _))
                when p = p1 || propertyMatch p p1 -> Expression.Constant(false, typeof<bool>) :> Expression
            | _ -> exp
        | Or' (innercontent) ->
            // All the following are decorated with Or'( ... )
            match innercontent with
            |(Not' (Or' (p2, p)), p1) 
                when p = p1 || propertyMatch p p1 -> Expression.OrElse(Expression.Not(p2), p1) :> Expression
            |(Not' (Or' (p, p2)), p1) 
            |(p1, Not' (Or' (p, p2))) 
            |(p1, Not' (Or' (p2, p))) 
                when p = p1 || propertyMatch p p1 -> Expression.OrElse(p1, Expression.Not(p2)) :> Expression
            |(Or' (_, p), Not' p1) 
            |(Or' (p, _), Not' p1) 
            |(Not' p1, Or' (_, p)) 
            |(Not' p1, Or' (p, _)) 
                when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
            |(Or' (_, Not' p), p1) 
            |(Or' (Not' p, _), p1) 
            |(p1, Or' (_, Not' p)) 
            |(p1, Or' (Not' p, _)) 
                when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
            |(Or'(_, Or' (_, p)), Not' p1) 
            |(Or'(_, Or' (p, _)), Not' p1) 
            |(Or'(Or' (p, _), _), Not' p1) 
            |(Or'(Or' (_, p), _), Not' p1) 
                when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
            |(Not' p1, Or'(_, Or' (_, p))) 
            |(Not' p1, Or'(_, Or' (p, _))) 
            |(Not' p1, Or'(Or' (p, _), _)) 
            |(Not' p1, Or'(Or' (_, p), _)) 
                when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
            |(Or' (Or'(_, Not' p), _), p1) 
            |(Or' (Or'(Not' p, _), _), p1) 
            |(p1, Or' (Or'(_, Not' p), _)) 
            |(p1, Or' (Or'(Not' p, _), _)) 
                when p = p1 || propertyMatch p p1 -> Expression.Constant(true, typeof<bool>) :> Expression
            | _ -> exp
        | noHit -> noHit

    let doubleNegation = function
        | Not' (Not' p) -> p
        | noHit -> noHit

    let deMorgan = function
        | Or' (Not' p, Not' p1) -> Expression.Not(Expression.AndAlso(p, p1)) :> Expression
        | And' (Not' p, Not' p1) -> Expression.Not(Expression.OrElse(p, p1)) :> Expression
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

    // A = "x" && A = "x" ->  A = "x"
    let ``remove duplicate condition`` (e:Expression) =
        // How deep we want to visit the tree? This is only 3 nodes.
        match e with 
        | And' inner ->
            match inner with
            | (ComparisonExpression (leftop, leftA, leftB), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(_,ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), And'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), And'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), And'(_,ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), And'(_, ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(And'(ComparisonExpression (rightop, rightA, rightB),_), _)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(And'(_,ComparisonExpression (rightop, rightA, rightB)), _)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(_, And'(ComparisonExpression (rightop, rightA, rightB),_))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), And'(_, And'(_,ComparisonExpression (rightop, rightA, rightB)))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And'(And'(ComparisonExpression (leftop, leftA, leftB), _), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And'(And'(_, ComparisonExpression (leftop, leftA, leftB)), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And'(_, And'(ComparisonExpression (leftop, leftA, leftB), _)), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (And'(_, And'(_, ComparisonExpression (leftop, leftA, leftB))), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | _ -> e
        | Or' inner ->
            match inner with
            | (ComparisonExpression (leftop, leftA, leftB), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_,ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), Or'(_,ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), Or'(_, ComparisonExpression (rightop, rightA, rightB))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(Or'(ComparisonExpression (rightop, rightA, rightB),_), _)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(Or'(_,ComparisonExpression (rightop, rightA, rightB)), _)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_, Or'(ComparisonExpression (rightop, rightA, rightB),_))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_, Or'(_,ComparisonExpression (rightop, rightA, rightB)))) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or'(Or'(ComparisonExpression (leftop, leftA, leftB), _), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or'(Or'(_, ComparisonExpression (leftop, leftA, leftB)), _), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or'(_, Or'(ComparisonExpression (leftop, leftA, leftB), _)), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | (Or'(_, Or'(_, ComparisonExpression (leftop, leftA, leftB))), ComparisonExpression (rightop, rightA, rightB)) when (leftop = rightop) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB)) -> fst inner
            | _ -> e
        | noHit -> noHit

    let private checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) =
        (
            (leftop = ExpressionType.Equal && rightop = ExpressionType.NotEqual) ||
            (leftop = ExpressionType.NotEqual && rightop = ExpressionType.Equal)
            ) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB) ||
                (leftA = rightB && leftB = rightA) || (propertyMatch leftA rightB && propertyMatch leftB rightA)
        ) || (
            (leftop = ExpressionType.LessThan && rightop = ExpressionType.GreaterThanOrEqual) ||
            (leftop = ExpressionType.GreaterThan && rightop = ExpressionType.LessThanOrEqual) ||
            (leftop = ExpressionType.GreaterThanOrEqual && rightop = ExpressionType.LessThan) ||
            (leftop = ExpressionType.LessThanOrEqual && rightop = ExpressionType.GreaterThan)
        ) && ((leftA = rightA && leftB = rightB) || (propertyMatch leftA rightA && propertyMatch leftB rightB))

    // A = "x" && A <> "x"  ->  false
    let ``remove mutually exclusive condition`` (e:Expression) =
        // How deep we want to visit the tree? This is only 3 nodes.
        match e with 
        | And' inner ->

            match inner with
            | (ComparisonExpression (leftop, leftA, leftB), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(_,ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), And'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), And'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (ComparisonExpression (leftop, leftA, leftB), _), And'(_,ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And' (_, ComparisonExpression (leftop, leftA, leftB)), And'(_, ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(And'(ComparisonExpression (rightop, rightA, rightB),_), _)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(And'(_,ComparisonExpression (rightop, rightA, rightB)), _)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(_, And'(ComparisonExpression (rightop, rightA, rightB),_))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), And'(_, And'(_,ComparisonExpression (rightop, rightA, rightB)))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And'(And'(ComparisonExpression (leftop, leftA, leftB), _), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And'(And'(_, ComparisonExpression (leftop, leftA, leftB)), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And'(_, And'(ComparisonExpression (leftop, leftA, leftB), _)), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | (And'(_, And'(_, ComparisonExpression (leftop, leftA, leftB))), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(false, typeof<bool>) :> Expression
            | _ -> e
        | Or' inner ->

            match inner with
            | (ComparisonExpression (leftop, leftA, leftB), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_,ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), Or'(ComparisonExpression (rightop, rightA, rightB),_)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (ComparisonExpression (leftop, leftA, leftB), _), Or'(_,ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or' (_, ComparisonExpression (leftop, leftA, leftB)), Or'(_, ComparisonExpression (rightop, rightA, rightB))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(Or'(ComparisonExpression (rightop, rightA, rightB),_), _)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(Or'(_,ComparisonExpression (rightop, rightA, rightB)), _)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_, Or'(ComparisonExpression (rightop, rightA, rightB),_))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (ComparisonExpression (leftop, leftA, leftB), Or'(_, Or'(_,ComparisonExpression (rightop, rightA, rightB)))) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(Or'(ComparisonExpression (leftop, leftA, leftB), _), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(Or'(_, ComparisonExpression (leftop, leftA, leftB)), _), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, Or'(ComparisonExpression (leftop, leftA, leftB), _)), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | (Or'(_, Or'(_, ComparisonExpression (leftop, leftA, leftB))), ComparisonExpression (rightop, rightA, rightB)) when checkMatch(leftop, leftA, leftB, rightop, rightA, rightB) -> Expression.Constant(true, typeof<bool>) :> Expression
            | _ -> e
        | noHit -> noHit

    // ------------------------------------- //
    /// Evaluating constants to not mess with our expressions:
    let ``evaluate constants`` (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.MemberAccess, ( :? MemberExpression as me) 
            when not(isNull me || isNull me.Expression) -> 
                match ``constant basic type`` me me.Expression with
                | Some x -> Expression.Constant(x, me.Type) :> Expression
                | _ -> e
        | ExpressionType.MemberAccess, ( :? MemberExpression as me) 
            when (not(isNull me)) && isNull me.Expression && 
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
    let ``evaluate basic constant math`` (e:Expression) =
        match e.NodeType, e with
        | ExpressionType.Add, (:? BinaryExpression as ce)
        | ExpressionType.AddChecked, (:? BinaryExpression as ce)
        | ExpressionType.Subtract, (:? BinaryExpression as ce)
        | ExpressionType.SubtractChecked, (:? BinaryExpression as ce)
        | ExpressionType.Multiply, (:? BinaryExpression as ce)
        | ExpressionType.MultiplyChecked, (:? BinaryExpression as ce)
        | ExpressionType.Divide, (:? BinaryExpression as ce)
        | ExpressionType.Modulo, (:? BinaryExpression as ce)
                 -> 
            if ce.Left.Type <> ce.Right.Type then e
            else
            match ce.Left.NodeType, ce.Right.NodeType, ce.Left, ce.Right with
            | ExpressionType.Constant, ExpressionType.Constant, (:? ConstantExpression as le), (:? ConstantExpression as ri) ->
                // F# doesn't support macros so this code is a bit copy-and-paste, but it should be trivial. 
                match e.NodeType with
                | ExpressionType.Add 
                | ExpressionType.AddChecked ->
                    match le.Value, ri.Value with
                    | (:? string  as lstr), (:? string  as rstr) when Type.(=) (le.Type, typeof<string>) -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (le.Type, typeof<decimal>) -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (le.Type, typeof<float32>) -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (le.Type, typeof<double>)  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (le.Type, typeof<float>)   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (le.Type, typeof<Int32>)   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (le.Type, typeof<Int64>)   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (le.Type, typeof<UInt32>)  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (le.Type, typeof<UInt64>)  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (le.Type, typeof<Int16>)   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (le.Type, typeof<UInt16>)  -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (le.Type, typeof<int8>)    -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (le.Type, typeof<uint8>)   -> Expression.Constant(lstr + rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Subtract
                | ExpressionType.SubtractChecked -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (le.Type, typeof<decimal>) -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (le.Type, typeof<float32>) -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (le.Type, typeof<double>)  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (le.Type, typeof<float>)   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (le.Type, typeof<Int32>)   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (le.Type, typeof<Int64>)   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (le.Type, typeof<UInt32>)  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (le.Type, typeof<UInt64>)  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (le.Type, typeof<Int16>)   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (le.Type, typeof<UInt16>)  -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (le.Type, typeof<int8>)    -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (le.Type, typeof<uint8>)   -> Expression.Constant(lstr - rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Multiply
                | ExpressionType.MultiplyChecked -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (le.Type, typeof<decimal>) -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (le.Type, typeof<float32>) -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (le.Type, typeof<double>)  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (le.Type, typeof<float>)   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (le.Type, typeof<Int32>)   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (le.Type, typeof<Int64>)   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (le.Type, typeof<UInt32>)  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (le.Type, typeof<UInt64>)  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (le.Type, typeof<Int16>)   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (le.Type, typeof<UInt16>)  -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (le.Type, typeof<int8>)    -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (le.Type, typeof<uint8>)   -> Expression.Constant(lstr * rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Divide -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (le.Type, typeof<decimal>) -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (le.Type, typeof<float32>) -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (le.Type, typeof<double>)  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (le.Type, typeof<float>)   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (le.Type, typeof<Int32>)   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (le.Type, typeof<Int64>)   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (le.Type, typeof<UInt32>)  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (le.Type, typeof<UInt64>)  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (le.Type, typeof<Int16>)   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (le.Type, typeof<UInt16>)  -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (le.Type, typeof<int8>)    -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (le.Type, typeof<uint8>)   -> Expression.Constant(lstr / rstr, le.Type) :> Expression 
                    | _ -> e
                | ExpressionType.Modulo -> 
                    match le.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (le.Type, typeof<decimal>) -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (le.Type, typeof<float32>) -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (le.Type, typeof<double>)  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (le.Type, typeof<float>)   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (le.Type, typeof<Int32>)   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (le.Type, typeof<Int64>)   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (le.Type, typeof<UInt32>)  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (le.Type, typeof<UInt64>)  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (le.Type, typeof<Int16>)   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (le.Type, typeof<UInt16>)  -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (le.Type, typeof<int8>)    -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (le.Type, typeof<uint8>)   -> Expression.Constant(lstr % rstr, le.Type) :> Expression 
                    | _ -> e
                | _ -> e
            | ExpressionType.Add, ExpressionType.Constant, (:? BinaryExpression as inner), (:? ConstantExpression as ri) when e.NodeType = ExpressionType.Add ->
                match inner.Left.NodeType, inner.Right.NodeType, inner.Left, inner.Right with
                // Strings only work on right-side:  x + "a" + "c" = x + "ac"
                | _, ExpressionType.Constant, _, (:? ConstantExpression as irc) when Type.(=) (irc.Type, typeof<string>) && Type.(=) (irc.Type, ri.Type) ->
                    // http://stackoverflow.com/questions/7027384/the-binary-operator-add-is-not-defined-for-the-types-system-string-and-syste
                    let concatMethod = typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |]); 
                    match irc.Value, ri.Value with
                    | (:? string  as lstr), (:? string  as rstr) when Type.(=) (irc.Type, typeof<string>)  -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type), concatMethod) :> Expression 
                    | _ -> e
                // ((x + 2) + 2) = x + 4 and ((2 + x) + 2) = x + 4 
                | ExpressionType.Constant, _, (:? ConstantExpression as irc), _
                | _, ExpressionType.Constant, _, (:? ConstantExpression as irc) when Type.(=) (irc.Type, ri.Type) ->
                    match irc.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (irc.Type, typeof<decimal>) -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (irc.Type, typeof<float32>) -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (irc.Type, typeof<double>)  -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (irc.Type, typeof<float>)   -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (irc.Type, typeof<Int32>)   -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (irc.Type, typeof<Int64>)   -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (irc.Type, typeof<UInt32>)  -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (irc.Type, typeof<UInt64>)  -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (irc.Type, typeof<Int16>)   -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (irc.Type, typeof<UInt16>)  -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (irc.Type, typeof<int8>)    -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (irc.Type, typeof<uint8>)   -> Expression.Add(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | _ -> e
                | _ -> e
            | ExpressionType.Subtract, ExpressionType.Constant, (:? BinaryExpression as inner), (:? ConstantExpression as ri) when e.NodeType = ExpressionType.Subtract ->
                // ((x - 2) - 2) = x - (2+2) but substarct can only be combined from the right side constants
                match inner.Right.NodeType, inner.Right with
                | ExpressionType.Constant, (:? ConstantExpression as irc) when Type.(=) (irc.Type, ri.Type) ->
                    match irc.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (irc.Type, typeof<decimal>) -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (irc.Type, typeof<float32>) -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (irc.Type, typeof<double>)  -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (irc.Type, typeof<float>)   -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (irc.Type, typeof<Int32>)   -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (irc.Type, typeof<Int64>)   -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (irc.Type, typeof<UInt32>)  -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (irc.Type, typeof<UInt64>)  -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (irc.Type, typeof<Int16>)   -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (irc.Type, typeof<UInt16>)  -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (irc.Type, typeof<int8>)    -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (irc.Type, typeof<uint8>)   -> Expression.Subtract(inner.Left, Expression.Constant(lstr + rstr, irc.Type)) :> Expression 
                    | _ -> e
                | _ -> e
            | ExpressionType.Multiply, ExpressionType.Constant, (:? BinaryExpression as inner), (:? ConstantExpression as ri) when e.NodeType = ExpressionType.Multiply ->
                // ((x * 2) * 2) = x * 4 and ((2 * x) * 2) = x * 4 
                match inner.Left.NodeType, inner.Right.NodeType, inner.Left, inner.Right with
                | ExpressionType.Constant, _, (:? ConstantExpression as irc), _ 
                | _, ExpressionType.Constant, _, (:? ConstantExpression as irc) when Type.(=) (irc.Type, ri.Type) ->
                    match irc.Value, ri.Value with
                    | (:? decimal as lstr), (:? decimal as rstr) when Type.(=) (irc.Type, typeof<decimal>) -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? float32 as lstr), (:? float32 as rstr) when Type.(=) (irc.Type, typeof<float32>) -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? double  as lstr), (:? double  as rstr) when Type.(=) (irc.Type, typeof<double>)  -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? float   as lstr), (:? float   as rstr) when Type.(=) (irc.Type, typeof<float>)   -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? Int32   as lstr), (:? Int32   as rstr) when Type.(=) (irc.Type, typeof<Int32>)   -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? Int64   as lstr), (:? Int64   as rstr) when Type.(=) (irc.Type, typeof<Int64>)   -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? UInt32  as lstr), (:? UInt32  as rstr) when Type.(=) (irc.Type, typeof<UInt32>)  -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? UInt64  as lstr), (:? UInt64  as rstr) when Type.(=) (irc.Type, typeof<UInt64>)  -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? Int16   as lstr), (:? Int16   as rstr) when Type.(=) (irc.Type, typeof<Int16>)   -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? UInt16  as lstr), (:? UInt16  as rstr) when Type.(=) (irc.Type, typeof<UInt16>)  -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? int8    as lstr), (:? int8    as rstr) when Type.(=) (irc.Type, typeof<int8>)    -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | (:? uint8   as lstr), (:? uint8   as rstr) when Type.(=) (irc.Type, typeof<uint8>)   -> Expression.Multiply(inner.Left, Expression.Constant(lstr * rstr, irc.Type)) :> Expression 
                    | _ -> e
                | _ -> e
            | _ -> e
        | _ -> e

// ------------------------------------- //
/// Used optimization methods
let mutable reductionMethods = [
     Methods.``evaluate constants``;  Methods.``evaluate basic constant math``
     Methods.``replace constant comparison``; Methods.``remove AnonymousType``; 
     Methods.``cut not used condition``; Methods.``not false is true``; Methods.``remove duplicate condition``; Methods.``remove mutually exclusive condition``; 
     (*Methods.associate; *) Methods.associate_complement; Methods.commute; (*Methods.commute2; Methods.distribute; *) Methods.commute_absorb; Methods.distribute_complement; Methods.gather; Methods.identity; 
     Methods.annihilate; Methods.absorb; Methods.idempotence; Methods.complement; Methods.doubleNegation; 
     Methods.deMorgan; Methods.balancetree]

/// Do reductions just for a current node?
let rec doReduction (exp:Expression) =
    if exp = Unchecked.defaultof<Expression> then exp else 
    let opt = Seq.fold(fun acc f -> f(acc)) exp reductionMethods

    match opt = exp with
    | true -> exp
    | false -> doReduction opt

// ------------------------------------- //

// Expression tree visitor: go through the whole expression tree.

// .NET has already this System.Linq.Expressions.Expressionvisitor
// Too bad this was so simple and faster than what it would have taken to get to know that 700 rows of source code!
[<TailCall>]
let rec internal visit' (exp:Expression): Expression =
    //bottom up:
    if isNull exp then null else
    doReduction (visitchilds exp)
    
and internal visitchilds (e:Expression): Expression =

    //printfn "%i" (System.Diagnostics.StackTrace().FrameCount)
    if isNull e then null else
    match e.NodeType, e with
    | ExpressionType.Constant, (:? ConstantExpression as e)    -> 
       let v = ``WhereSelectEnumerableIterator visitor`` e
       upcast v
    | ExpressionType.Parameter, (:? ParameterExpression as e)   -> upcast e
    | ExpressionType.MemberAccess, (:? MemberExpression as e)      -> 
        let v = visit' e.Expression
        if v=e.Expression then upcast e else upcast Expression.MakeMemberAccess(v, e.Member)
    | ExpressionType.Call, (:? MethodCallExpression as e)  -> 
        let obje = visit' e.Object
        let args = e.Arguments |> Seq.toArray 
        let visited = args |> Array.map visit'
        if e.Object = obje && args = visited then upcast e else
        upcast Expression.Call(obje, e.Method, visited)
    | ExpressionType.Lambda, (:? LambdaExpression as e)      -> 
        let b = visit' e.Body 
        if b=e.Body then upcast e 
        else 
        upcast Expression.Lambda(e.Type, visit' b, e.Parameters) // intentional revisit
    | ExpressionType.TypeIs, (:? TypeBinaryExpression as e)  -> 
        let v = visit'(e.Expression)
        if v=e.Expression then upcast e else upcast Expression.TypeIs(v, e.TypeOperand)
    | ExpressionType.Conditional, (:? ConditionalExpression as e) -> 
        let v1, v2, v3 = visit' e.Test, visit' e.IfTrue, visit' e.IfFalse
        if v1=e.Test && v2=e.IfTrue && v3=e.IfFalse then upcast e else upcast Expression.Condition(v1, v2, v3)
    | ExpressionType.New, (:? NewExpression as e) -> 
        if isNull e.Members then
            upcast Expression.New(e.Constructor, e.Arguments |> Seq.map visit')
        else
            upcast Expression.New(e.Constructor, (e.Arguments |> Seq.map visit'), e.Members)
    | ExpressionType.NewArrayBounds, (:? NewArrayExpression as e) ->
                                         upcast Expression.NewArrayBounds(e.Type.GetElementType(), e.Expressions |> Seq.map visit')
    | ExpressionType.NewArrayInit, (:? NewArrayExpression as e)    -> upcast Expression.NewArrayInit(e.Type.GetElementType(), e.Expressions |> Seq.map visit')
    | ExpressionType.Invoke, (:? InvocationExpression as e)  -> upcast Expression.Invoke(visit' e.Expression, e.Arguments |> Seq.map visit')
    | ExpressionType.MemberInit, (:? MemberInitExpression as e)  -> upcast Expression.MemberInit( (visit' e.NewExpression) :?> NewExpression , e.Bindings) //probably shoud visit' also bindings
    | ExpressionType.ListInit, (:? ListInitExpression as e)    -> upcast Expression.ListInit( (visit' e.NewExpression) :?> NewExpression, e.Initializers) //probably shoud visit' also initialixers
    | _, (:? UnaryExpression as e) -> 
        let visit'ed = visit' e.Operand
        if visit'ed=e.Operand then upcast e else upcast Expression.MakeUnary(e.NodeType,visit'ed,e.Type,e.Method) 
    | ExpressionType.Coalesce, (:? BinaryExpression as e) when  not (isNull e.Conversion) -> 
            let v1, v2, v3 = visit' e.Left, visit' e.Right, visit' e.Conversion
            if v1=e.Left && v2=e.Right && v3=(e.Conversion:>Expression) then upcast e else upcast Expression.Coalesce(v1, v2, v3 :?> LambdaExpression)
    | nt, (:? BinaryExpression as e)      -> 
            let v1, v2 = visit' e.Left, visit' e.Right
            if v1=e.Left && v2=e.Right then upcast e else upcast Expression.MakeBinary(nt,v1,v2,e.IsLiftedToNull, e.Method)
    | nt, _ -> if (int nt = 52) then e // Expression.Extension
               else failwith ("encountered unknown LINQ expression: " + e.NodeType.ToString() + " " + e.ToString())

// Look also inside a LINQ-wrapper
// https://referencesource.microsoft.com/#System.Core/System/Linq/Enumerable.cs,8bf16962931637d3,references
and internal ``WhereSelectEnumerableIterator visitor`` (ce:ConstantExpression) : ConstantExpression =
    if isNull ce.Value || isNull ce.Type || (not (ce.Type.FullName.StartsWith "System.Linq")) then ce
    else
    let enuProp = ce.Type.GetProperty("Enumerable", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    if isNull enuProp then ce
    else
    let enu = enuProp.GetValue(ce.Value, null)
    if isNull enu then ce
    else
    let srcProp = enu.GetType().GetField("source", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    if isNull srcProp then ce
    else 
    let src = srcProp.GetValue enu
    if isNull src then ce
    else
    let exprItm = src.GetType().GetField("expression", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    let expr = exprItm.GetValue src
    if isNull expr then ce
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

/// Expression tree visitor: go through the whole expression tree.
let visitTyped<'T>(exp:Expression<'T>) =
    visit' exp :?> Expression<'T>

/// Expression tree visitor: go through the whole expression tree.
/// Catches the exceptions.
let tryVisitTyped<'T>(exp:Expression<'T>) =
    try visit' exp :?> Expression<'T>
    with _ -> exp
