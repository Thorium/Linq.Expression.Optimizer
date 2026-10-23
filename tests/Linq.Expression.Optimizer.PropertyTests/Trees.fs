/// Random expression trees for the optimizer properties: booleans, integers,
/// doubles (NaN included), nullable integers and strings over ten
/// parameters, with the shapes every reduction rule is written for. A tree
/// is built into a System.Linq.Expressions tree and compiled against a
/// random environment, so the compiled original is the oracle for the
/// compiled optimized tree.
module Linq.Expression.Optimizer.PropertyTests.Trees

open System
open System.Linq.Expressions
open System.Reflection
open FsCheck
open FsCheck.FSharp

// ============================================================================
// The tree
// ============================================================================

type Cmp =
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Ne

type BExpr =
    /// p or q
    | BVar of int
    | BConst of bool
    | Not of BExpr
    | AndAlso of a: BExpr * b: BExpr
    | OrElse of a: BExpr * b: BExpr
    /// non-short-circuit and / or / xor on booleans
    | BAnd of a: BExpr * b: BExpr
    | BOr of a: BExpr * b: BExpr
    | BXor of a: BExpr * b: BExpr
    | BEq of a: BExpr * b: BExpr
    | BNe of a: BExpr * b: BExpr
    | ICmp of c: Cmp * a: IExpr * b: IExpr
    | DCmp of c: Cmp * a: DExpr * b: DExpr
    /// lifted comparison of nullable ints: null compares false
    | NCmp of c: Cmp * a: NExpr * b: NExpr
    | SEq of a: SExpr * b: SExpr
    | SEmpty of SExpr
    | NHasValue of NExpr
    | BIf of c: BExpr * a: BExpr * b: BExpr

and IExpr =
    /// x, y or z
    | IVar of int
    | IConst of int
    | Add of a: IExpr * b: IExpr
    | Sub of a: IExpr * b: IExpr
    | Mul of a: IExpr * b: IExpr
    | Div of a: IExpr * b: IExpr
    | Mod of a: IExpr * b: IExpr
    | Neg of IExpr
    | IIf of c: BExpr * a: IExpr * b: IExpr
    /// a field read off a captured object (constant-evaluated by the optimizer)
    | Captured of int
    | SLen of SExpr
    | NCoalesce of a: NExpr * b: IExpr
    | NValue of NExpr

and DExpr =
    | DVar
    | DConst of float
    | DAdd of a: DExpr * b: DExpr
    | DMul of a: DExpr * b: DExpr
    | DIf of c: BExpr * a: DExpr * b: DExpr

and NExpr =
    /// nx or ny
    | NVar of int
    | NConst of int option
    | NAdd of a: NExpr * b: NExpr
    | NIf of c: BExpr * a: NExpr * b: NExpr

and SExpr =
    /// s or t
    | SVar of int
    | SConst of string
    | SConcat of a: SExpr * b: SExpr
    | SIf of c: BExpr * a: SExpr * b: SExpr

/// What the parameters hold when a tree is evaluated.
type Env =
    { X: int
      Y: int
      Z: int
      P: bool
      Q: bool
      S: string
      T: string
      NX: Nullable<int>
      NY: Nullable<int>
      D: float }

/// The object a Captured field reads from.
type Holder(value: int) =
    member val Value = value with get, set

// ============================================================================
// Building the System.Linq.Expressions tree
// ============================================================================

let px = Expression.Parameter(typeof<int>, "x")
let py = Expression.Parameter(typeof<int>, "y")
let pz = Expression.Parameter(typeof<int>, "z")
let pp = Expression.Parameter(typeof<bool>, "p")
let pq = Expression.Parameter(typeof<bool>, "q")
let ps = Expression.Parameter(typeof<string>, "s")
let pt = Expression.Parameter(typeof<string>, "t")
let pnx = Expression.Parameter(typeof<Nullable<int>>, "nx")
let pny = Expression.Parameter(typeof<Nullable<int>>, "ny")
let pd = Expression.Parameter(typeof<float>, "d")

let parameters = [| px; py; pz; pp; pq; ps; pt; pnx; pny; pd |]

let private cmp (c: Cmp) (l: Expression) (r: Expression) : Expression =
    match c with
    | Lt -> Expression.LessThan(l, r)
    | Le -> Expression.LessThanOrEqual(l, r)
    | Gt -> Expression.GreaterThan(l, r)
    | Ge -> Expression.GreaterThanOrEqual(l, r)
    | Eq -> Expression.Equal(l, r)
    | Ne -> Expression.NotEqual(l, r)

let private concatMethod = typeof<string>.GetMethod("Concat", [| typeof<string>; typeof<string> |])
let private isNullOrEmptyMethod = typeof<string>.GetMethod("IsNullOrEmpty", [| typeof<string> |])
let private stringEqualsMethod = typeof<string>.GetMethod("op_Equality", [| typeof<string>; typeof<string> |])
let private lengthProperty = typeof<string>.GetProperty "Length"
let private valueProperty = typeof<Holder>.GetProperty "Value"

let rec buildB (e: BExpr) : Expression =
    match e with
    | BVar 0 -> pp
    | BVar _ -> pq
    | BConst b -> Expression.Constant(b, typeof<bool>)
    | Not a -> Expression.Not(buildB a)
    | AndAlso (a, b) -> Expression.AndAlso(buildB a, buildB b)
    | OrElse (a, b) -> Expression.OrElse(buildB a, buildB b)
    | BAnd (a, b) -> Expression.And(buildB a, buildB b)
    | BOr (a, b) -> Expression.Or(buildB a, buildB b)
    | BXor (a, b) -> Expression.ExclusiveOr(buildB a, buildB b)
    | BEq (a, b) -> Expression.Equal(buildB a, buildB b)
    | BNe (a, b) -> Expression.NotEqual(buildB a, buildB b)
    | ICmp (c, a, b) -> cmp c (buildI a) (buildI b)
    | DCmp (c, a, b) -> cmp c (buildD a) (buildD b)
    | NCmp (c, a, b) -> cmp c (buildN a) (buildN b)
    | SEq (a, b) -> Expression.Equal(buildS a, buildS b, false, stringEqualsMethod)
    | SEmpty a -> Expression.Call(isNullOrEmptyMethod, buildS a)
    | NHasValue a -> Expression.Property(buildN a, "HasValue")
    | BIf (c, a, b) -> Expression.Condition(buildB c, buildB a, buildB b)

and buildI (e: IExpr) : Expression =
    match e with
    | IVar 0 -> px
    | IVar 1 -> py
    | IVar _ -> pz
    | IConst n -> Expression.Constant(n, typeof<int>)
    | Add (a, b) -> Expression.Add(buildI a, buildI b)
    | Sub (a, b) -> Expression.Subtract(buildI a, buildI b)
    | Mul (a, b) -> Expression.Multiply(buildI a, buildI b)
    | Div (a, b) -> Expression.Divide(buildI a, buildI b)
    | Mod (a, b) -> Expression.Modulo(buildI a, buildI b)
    | Neg a -> Expression.Negate(buildI a)
    | IIf (c, a, b) -> Expression.Condition(buildB c, buildI a, buildI b)
    | Captured n -> Expression.Property(Expression.Constant(Holder n), valueProperty)
    | SLen a -> Expression.Property(buildS a, lengthProperty)
    | NCoalesce (a, b) -> Expression.Coalesce(buildN a, buildI b)
    | NValue a -> Expression.Property(buildN a, "Value")

and buildD (e: DExpr) : Expression =
    match e with
    | DVar -> pd
    | DConst f -> Expression.Constant(f, typeof<float>)
    | DAdd (a, b) -> Expression.Add(buildD a, buildD b)
    | DMul (a, b) -> Expression.Multiply(buildD a, buildD b)
    | DIf (c, a, b) -> Expression.Condition(buildB c, buildD a, buildD b)

and buildN (e: NExpr) : Expression =
    match e with
    | NVar 0 -> pnx
    | NVar _ -> pny
    | NConst (Some n) -> Expression.Constant(Nullable n, typeof<Nullable<int>>)
    | NConst None -> Expression.Constant(null, typeof<Nullable<int>>)
    | NAdd (a, b) -> Expression.Add(buildN a, buildN b)
    | NIf (c, a, b) -> Expression.Condition(buildB c, buildN a, buildN b)

and buildS (e: SExpr) : Expression =
    match e with
    | SVar 0 -> ps
    | SVar _ -> pt
    | SConst s -> Expression.Constant(s, typeof<string>)
    | SConcat (a, b) -> Expression.Add(buildS a, buildS b, concatMethod)
    | SIf (c, a, b) -> Expression.Condition(buildB c, buildS a, buildS b)

/// One of the typed roots, so every kind of tree is checked as a whole.
type Tree =
    | TB of BExpr
    | TI of IExpr
    | TD of DExpr
    | TN of NExpr
    | TS of SExpr

let build (tree: Tree) : Expression =
    match tree with
    | TB e -> buildB e
    | TI e -> buildI e
    | TD e -> buildD e
    | TN e -> buildN e
    | TS e -> buildS e

// ============================================================================
// Evaluation
// ============================================================================

/// The value of a tree in an environment, or the exception it throws.
type Outcome =
    | Value of string
    | Throws of string

let private describe (o: obj) =
    match o with
    | null -> "null"
    | :? float as f -> f.ToString "R"
    | :? string as s -> $"\"{s}\""
    | o -> o.ToString()

/// Compile a body over all ten parameters. Compiling can fail on its own
/// (a tree the optimizer left referring to a parameter it no longer binds,
/// for one), so the compiled function is returned as a Result.
let compile (body: Expression) : Result<obj[] -> Outcome, string> =
    try
        let lambda = Expression.Lambda(body, parameters)
        let f = lambda.Compile()
        Ok(fun args ->
            try
                Value(describe (f.DynamicInvoke args))
            with
            | :? TargetInvocationException as e -> Throws(e.InnerException.GetType().Name)
            | e -> Throws(e.GetType().Name))
    with e -> Error(e.GetType().Name + ": " + e.Message)

let args (env: Env) : obj[] =
    [| box env.X; box env.Y; box env.Z; box env.P; box env.Q; box env.S; box env.T; box env.NX; box env.NY; box env.D |]

// ============================================================================
// Generation
// ============================================================================

let private genCmp = Gen.elements [ Lt; Le; Gt; Ge; Eq; Ne ]

let private genIntConst = Gen.frequency [ 3, Gen.elements [ 0; 1; -1; 2; 10 ]; 1, Gen.choose (-100, 100); 1, Gen.elements [ Int32.MaxValue; Int32.MinValue ] ]

let private genDoubleConst =
    Gen.frequency
        [ 3, Gen.elements [ 0.0; 1.0; -1.0; 2.5; -0.0 ]
          2, Gen.elements [ Double.NaN; Double.PositiveInfinity; Double.NegativeInfinity ]
          1, Gen.choose (-1000, 1000) |> Gen.map (fun n -> float n / 4.0) ]

let private genStringConst = Gen.elements [ ""; "a"; "ab"; "b"; " "; null ]

let rec genB (depth: int) : Gen<BExpr> =
    let leaf =
        Gen.frequency
            [ 4, Gen.choose (0, 1) |> Gen.map BVar
              2, Gen.elements [ BConst true; BConst false ] ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let i = genI (depth - 1)
        let d = genD (depth - 1)
        let n = genN (depth - 1)
        let s = genS (depth - 1)
        Gen.frequency
            [ 3, leaf
              3, Gen.map Not b
              4, Gen.zip b b |> Gen.map AndAlso
              4, Gen.zip b b |> Gen.map OrElse
              1, Gen.zip b b |> Gen.map BAnd
              1, Gen.zip b b |> Gen.map BOr
              1, Gen.zip b b |> Gen.map BXor
              1, Gen.zip b b |> Gen.map BEq
              1, Gen.zip b b |> Gen.map BNe
              4, Gen.zip3 genCmp i i |> Gen.map ICmp
              2, Gen.zip3 genCmp d d |> Gen.map DCmp
              2, Gen.zip3 genCmp n n |> Gen.map NCmp
              1, Gen.zip s s |> Gen.map SEq
              1, Gen.map SEmpty s
              1, Gen.map NHasValue n
              1, Gen.zip3 b b b |> Gen.map BIf
              6, genShaped (depth - 1) ]

and genI (depth: int) : Gen<IExpr> =
    let leaf =
        Gen.frequency
            [ 4, Gen.choose (0, 2) |> Gen.map IVar
              3, Gen.map IConst genIntConst
              1, Gen.map Captured genIntConst ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let i = genI (depth - 1)
        let n = genN (depth - 1)
        let s = genS (depth - 1)
        Gen.frequency
            [ 4, leaf
              3, Gen.zip i i |> Gen.map Add
              3, Gen.zip i i |> Gen.map Sub
              3, Gen.zip i i |> Gen.map Mul
              2, Gen.zip i i |> Gen.map Div
              2, Gen.zip i i |> Gen.map Mod
              1, Gen.map Neg i
              2, Gen.zip3 b i i |> Gen.map IIf
              1, Gen.map SLen s
              1, Gen.zip n i |> Gen.map NCoalesce
              1, Gen.map NValue n ]

and genD (depth: int) : Gen<DExpr> =
    let leaf = Gen.frequency [ 3, Gen.constant DVar; 3, Gen.map DConst genDoubleConst ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let d = genD (depth - 1)
        Gen.frequency
            [ 4, leaf
              2, Gen.zip d d |> Gen.map DAdd
              2, Gen.zip d d |> Gen.map DMul
              1, Gen.zip3 b d d |> Gen.map DIf ]

and genN (depth: int) : Gen<NExpr> =
    let leaf =
        Gen.frequency
            [ 3, Gen.choose (0, 1) |> Gen.map NVar
              2, Gen.map (Some >> NConst) genIntConst
              1, Gen.constant (NConst None) ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let n = genN (depth - 1)
        Gen.frequency [ 4, leaf; 2, Gen.zip n n |> Gen.map NAdd; 1, Gen.zip3 b n n |> Gen.map NIf ]

and genS (depth: int) : Gen<SExpr> =
    let leaf = Gen.frequency [ 3, Gen.choose (0, 1) |> Gen.map SVar; 2, Gen.map SConst genStringConst ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let s = genS (depth - 1)
        Gen.frequency
            [ 4, leaf
              2, Gen.zip s s |> Gen.map SConcat
              1, Gen.zip3 b s s |> Gen.map SIf
              // an empty string beside a concatenation: the shape the empty-string rule
              // may drop (a concatenation is never null, a bare parameter may be)
              1, Gen.zip s s |> Gen.map (fun (a, c) -> SConcat (SConcat (a, c), SConst ""))
              1, Gen.zip s s |> Gen.map (fun (a, c) -> SConcat (SConst "", SConcat (a, c))) ]

/// The shapes the algebraic rules are written for, which random trees
/// seldom hit because they need the same subtree twice: idempotence and
/// complement (a and a, a or not a), gathering and complement
/// distribution ((a or b) and (a or c), (a and b) or (a and not b)),
/// comparison ranges on one variable (x > 1 and x > 2), duplicated and
/// mutually exclusive comparisons, and long and/or chains for balancing.
and genShaped (depth: int) : Gen<BExpr> =
    let b = genB (max 0 depth)
    let variable = Gen.choose (0, 2) |> Gen.map IVar
    let range =
        gen {
            let! v = variable
            let! op1 = genCmp
            let! op2 = genCmp
            let! k1 = genIntConst
            let! k2 = genIntConst
            let! join = Gen.elements [ AndAlso; OrElse ]
            return join (ICmp (op1, v, IConst k1), ICmp (op2, v, IConst k2))
        }
    let exclusive =
        gen {
            let! v = variable
            let! k = genIntConst
            let! op1, op2 = Gen.elements [ Eq, Ne; Ne, Eq; Lt, Ge; Gt, Le; Ge, Lt; Le, Gt ]
            let! join = Gen.elements [ AndAlso; OrElse ]
            return join (ICmp (op1, v, IConst k), ICmp (op2, v, IConst k))
        }
    let chain =
        gen {
            let! leaves = Gen.listOfLength 8 (genB 0)
            let! join = Gen.elements [ AndAlso; OrElse ]
            let! rightNested = Gen.elements [ true; false ]
            return
                if rightNested then leaves |> List.reduceBack (fun l r -> join (l, r))
                else leaves |> List.reduce (fun l r -> join (l, r))
        }
    Gen.frequency
        [ 2, Gen.map (fun a -> AndAlso (a, a)) b
          2, Gen.map (fun a -> OrElse (a, a)) b
          2, Gen.map (fun a -> AndAlso (a, Not a)) b
          2, Gen.map (fun a -> OrElse (Not a, a)) b
          2, Gen.zip3 b b b |> Gen.map (fun (a, x, y) -> AndAlso (OrElse (a, x), OrElse (a, y)))
          2, Gen.zip3 b b b |> Gen.map (fun (a, x, y) -> OrElse (AndAlso (a, x), AndAlso (a, y)))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> OrElse (AndAlso (a, x), AndAlso (a, Not x)))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> AndAlso (OrElse (a, x), OrElse (a, Not x)))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> AndAlso (a, OrElse (a, x)))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> OrElse (a, AndAlso (Not a, x)))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> AndAlso (a, Not (AndAlso (a, x))))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> AndAlso (AndAlso (a, x), Not a))
          2, Gen.zip b b |> Gen.map (fun (a, x) -> OrElse (Not (OrElse (x, a)), a))
          4, range
          3, exclusive
          2, chain ]
let genTree : Gen<Tree> =
    Gen.sized (fun size ->
        let depth = min 5 (1 + size / 15)
        Gen.frequency
            [ 6, Gen.map TB (genB depth)
              4, Gen.map TI (genI depth)
              1, Gen.map TD (genD depth)
              1, Gen.map TN (genN depth)
              1, Gen.map TS (genS depth) ])

// ============================================================================
// Shrinking: a node shrinks to any of its children of the same type
// ============================================================================

let rec shrinkB (e: BExpr) : seq<BExpr> =
    seq {
        match e with
        | BVar _ -> ()
        | BConst _ -> ()
        | Not a -> yield a; yield! shrinkB a |> Seq.map Not
        | AndAlso (a, b) -> yield a; yield b; yield! shrinkB a |> Seq.map (fun a' -> AndAlso (a', b)); yield! shrinkB b |> Seq.map (fun b' -> AndAlso (a, b'))
        | OrElse (a, b) -> yield a; yield b; yield! shrinkB a |> Seq.map (fun a' -> OrElse (a', b)); yield! shrinkB b |> Seq.map (fun b' -> OrElse (a, b'))
        | BAnd (a, b) | BOr (a, b) | BXor (a, b) | BEq (a, b) | BNe (a, b) -> yield a; yield b
        | ICmp (c, a, b) -> yield! shrinkI a |> Seq.map (fun a' -> ICmp (c, a', b)); yield! shrinkI b |> Seq.map (fun b' -> ICmp (c, a, b'))
        | DCmp (c, a, b) -> yield! shrinkD a |> Seq.map (fun a' -> DCmp (c, a', b)); yield! shrinkD b |> Seq.map (fun b' -> DCmp (c, a, b'))
        | NCmp (c, a, b) -> yield! shrinkN a |> Seq.map (fun a' -> NCmp (c, a', b)); yield! shrinkN b |> Seq.map (fun b' -> NCmp (c, a, b'))
        | SEq (a, b) -> yield! shrinkS a |> Seq.map (fun a' -> SEq (a', b)); yield! shrinkS b |> Seq.map (fun b' -> SEq (a, b'))
        | SEmpty a -> yield! shrinkS a |> Seq.map SEmpty
        | NHasValue a -> yield! shrinkN a |> Seq.map NHasValue
        | BIf (c, a, b) -> yield c; yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> BIf (c', a, b))
    }

and shrinkI (e: IExpr) : seq<IExpr> =
    seq {
        match e with
        | IVar _ | IConst _ | Captured _ -> ()
        | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Mod (a, b) ->
            yield a
            yield b
            let make a' b' =
                match e with
                | Add _ -> Add (a', b')
                | Sub _ -> Sub (a', b')
                | Mul _ -> Mul (a', b')
                | Div _ -> Div (a', b')
                | _ -> Mod (a', b')
            yield! shrinkI a |> Seq.map (fun a' -> make a' b)
            yield! shrinkI b |> Seq.map (fun b' -> make a b')
        | Neg a -> yield a; yield! shrinkI a |> Seq.map Neg
        | IIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> IIf (c', a, b))
        | SLen a -> yield! shrinkS a |> Seq.map SLen
        | NCoalesce (a, b) -> yield b; yield! shrinkN a |> Seq.map (fun a' -> NCoalesce (a', b))
        | NValue a -> yield! shrinkN a |> Seq.map NValue
    }

and shrinkD (e: DExpr) : seq<DExpr> =
    seq {
        match e with
        | DVar | DConst _ -> ()
        | DAdd (a, b) | DMul (a, b) -> yield a; yield b
        | DIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> DIf (c', a, b))
    }

and shrinkN (e: NExpr) : seq<NExpr> =
    seq {
        match e with
        | NVar _ | NConst _ -> ()
        | NAdd (a, b) -> yield a; yield b
        | NIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> NIf (c', a, b))
    }

and shrinkS (e: SExpr) : seq<SExpr> =
    seq {
        match e with
        | SVar _ | SConst _ -> ()
        | SConcat (a, b) -> yield a; yield b
        | SIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> SIf (c', a, b))
    }

let shrinkTree (tree: Tree) : seq<Tree> =
    match tree with
    | TB e -> shrinkB e |> Seq.map TB
    | TI e -> shrinkI e |> Seq.map TI
    | TD e -> shrinkD e |> Seq.map TD
    | TN e -> shrinkN e |> Seq.map TN
    | TS e -> shrinkS e |> Seq.map TS

let arbTree : Arbitrary<Tree> = Arb.fromGenShrink (genTree, shrinkTree)

// ============================================================================
// Environments
// ============================================================================

let private genNullable = Gen.frequency [ 1, Gen.constant (Nullable<int>()); 3, Gen.map Nullable genIntConst ]

let genEnv : Gen<Env> =
    gen {
        let! x = genIntConst
        let! y = genIntConst
        let! z = genIntConst
        let! p = Gen.elements [ true; false ]
        let! q = Gen.elements [ true; false ]
        let! s = genStringConst
        let! t = genStringConst
        let! nx = genNullable
        let! ny = genNullable
        let! d = genDoubleConst
        return { X = x; Y = y; Z = z; P = p; Q = q; S = s; T = t; NX = nx; NY = ny; D = d }
    }

/// A fixed grid of environments every tree is checked on, so a failure is
/// reproducible from the tree alone.
let envs : Env list =
    let ints = [ 0; 1; -1; 2; Int32.MaxValue ]
    let strings = [ ""; "a"; null ]
    let nullables = [ Nullable<int>(); Nullable 0; Nullable 1 ]
    let doubles = [ 0.0; 1.0; Double.NaN ]
    [ for i in 0 .. 47 ->
          { X = ints.[i % 5]
            Y = ints.[(i / 5) % 5]
            Z = ints.[(i / 25) % 5]
            P = i % 2 = 0
            Q = (i / 2) % 2 = 0
            S = strings.[i % 3]
            T = strings.[(i / 3) % 3]
            NX = nullables.[(i / 7) % 3]
            NY = nullables.[(i / 11) % 3]
            D = doubles.[(i / 4) % 3] } ]
