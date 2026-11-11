/// Random expression trees for the optimizer properties: booleans, integers,
/// doubles (NaN included), decimals, nullable integers, strings and an object
/// that may be null over fifteen parameters (two of them named alike, two
/// unnamed), with the shapes every reduction rule is written for. A tree
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
    /// p, q or an unnamed parameter
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
    | MCmp of c: Cmp * a: MExpr * b: MExpr
    /// o.Flag, o == null and o != null over an object that may be null
    | OFlag
    | OIsNull
    | ONotNull
    | SEq of a: SExpr * b: SExpr
    | SEmpty of SExpr
    | NHasValue of NExpr
    | BIf of c: BExpr * a: BExpr * b: BExpr

and IExpr =
    /// x, y, z, a second parameter named x or an unnamed one
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
    /// o.Value: throws when o is null
    | OValue

and DExpr =
    | DVar
    | DConst of float
    | DAdd of a: DExpr * b: DExpr
    | DSub of a: DExpr * b: DExpr
    | DMul of a: DExpr * b: DExpr
    | DDiv of a: DExpr * b: DExpr
    | DIf of c: BExpr * a: DExpr * b: DExpr

and NExpr =
    /// nx or ny
    | NVar of int
    | NConst of int option
    | NAdd of a: NExpr * b: NExpr
    | NSub of a: NExpr * b: NExpr
    | NMul of a: NExpr * b: NExpr
    | NMod of a: NExpr * b: NExpr
    | NIf of c: BExpr * a: NExpr * b: NExpr

and MExpr =
    | MVar
    | MConst of decimal
    | MAdd of a: MExpr * b: MExpr
    | MSub of a: MExpr * b: MExpr
    | MMul of a: MExpr * b: MExpr
    | MDiv of a: MExpr * b: MExpr
    | MIf of c: BExpr * a: MExpr * b: MExpr

and SExpr =
    /// s or t
    | SVar of int
    | SConst of string
    | SConcat of a: SExpr * b: SExpr
    | SIf of c: BExpr * a: SExpr * b: SExpr

/// The object a Captured field reads from, and the one the parameter o holds
/// (or null).
[<AllowNullLiteral>]
type Holder(value: int) =
    member val Value = value with get, set
    member _.Flag = value > 0

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
      D: float
      /// the second parameter named x
      X2: int
      /// the unnamed int and bool parameters
      U: int
      V: bool
      M: decimal
      O: Holder }

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
/// Another parameter named x: the same operand as px by name, not by reference.
let px2 = Expression.Parameter(typeof<int>, "x")
let pu = Expression.Parameter typeof<int>
let pv = Expression.Parameter typeof<bool>
let pm = Expression.Parameter(typeof<decimal>, "m")
let po = Expression.Parameter(typeof<Holder>, "o")

let parameters = [| px; py; pz; pp; pq; ps; pt; pnx; pny; pd; px2; pu; pv; pm; po |]

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
let private flagProperty = typeof<Holder>.GetProperty "Flag"
let private noHolder = Expression.Constant(null, typeof<Holder>)

let rec buildB (e: BExpr) : Expression =
    match e with
    | BVar 0 -> pp
    | BVar 1 -> pq
    | BVar _ -> pv
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
    | MCmp (c, a, b) -> cmp c (buildM a) (buildM b)
    | OFlag -> Expression.Property(po, flagProperty)
    | OIsNull -> Expression.Equal(po, noHolder)
    | ONotNull -> Expression.NotEqual(po, noHolder)
    | SEq (a, b) -> Expression.Equal(buildS a, buildS b, false, stringEqualsMethod)
    | SEmpty a -> Expression.Call(isNullOrEmptyMethod, buildS a)
    | NHasValue a -> Expression.Property(buildN a, "HasValue")
    | BIf (c, a, b) -> Expression.Condition(buildB c, buildB a, buildB b)

and buildI (e: IExpr) : Expression =
    match e with
    | IVar 0 -> px
    | IVar 1 -> py
    | IVar 2 -> pz
    | IVar 3 -> px2
    | IVar _ -> pu
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
    | OValue -> Expression.Property(po, valueProperty)

and buildD (e: DExpr) : Expression =
    match e with
    | DVar -> pd
    | DConst f -> Expression.Constant(f, typeof<float>)
    | DAdd (a, b) -> Expression.Add(buildD a, buildD b)
    | DSub (a, b) -> Expression.Subtract(buildD a, buildD b)
    | DMul (a, b) -> Expression.Multiply(buildD a, buildD b)
    | DDiv (a, b) -> Expression.Divide(buildD a, buildD b)
    | DIf (c, a, b) -> Expression.Condition(buildB c, buildD a, buildD b)

and buildN (e: NExpr) : Expression =
    match e with
    | NVar 0 -> pnx
    | NVar _ -> pny
    | NConst (Some n) -> Expression.Constant(Nullable n, typeof<Nullable<int>>)
    | NConst None -> Expression.Constant(null, typeof<Nullable<int>>)
    | NAdd (a, b) -> Expression.Add(buildN a, buildN b)
    | NSub (a, b) -> Expression.Subtract(buildN a, buildN b)
    | NMul (a, b) -> Expression.Multiply(buildN a, buildN b)
    | NMod (a, b) -> Expression.Modulo(buildN a, buildN b)
    | NIf (c, a, b) -> Expression.Condition(buildB c, buildN a, buildN b)

and buildM (e: MExpr) : Expression =
    match e with
    | MVar -> pm
    | MConst m -> Expression.Constant(m, typeof<decimal>)
    | MAdd (a, b) -> Expression.Add(buildM a, buildM b)
    | MSub (a, b) -> Expression.Subtract(buildM a, buildM b)
    | MMul (a, b) -> Expression.Multiply(buildM a, buildM b)
    | MDiv (a, b) -> Expression.Divide(buildM a, buildM b)
    | MIf (c, a, b) -> Expression.Condition(buildB c, buildM a, buildM b)

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
    | TM of MExpr

let build (tree: Tree) : Expression =
    match tree with
    | TB e -> buildB e
    | TI e -> buildI e
    | TD e -> buildD e
    | TN e -> buildN e
    | TS e -> buildS e
    | TM e -> buildM e

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
    // a decimal is the same by value, whatever its scale: 6.00 is 6
    | :? decimal as d -> d.ToString("G29", System.Globalization.CultureInfo.InvariantCulture)
    | :? string as s -> $"\"{s}\""
    | o -> o.ToString()

/// Compile a body over all the parameters. Compiling can fail on its own
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
    [| box env.X; box env.Y; box env.Z; box env.P; box env.Q; box env.S; box env.T; box env.NX; box env.NY; box env.D
       box env.X2; box env.U; box env.V; box env.M; box env.O |]

// ============================================================================
// Generation
// ============================================================================

let private genCmp = Gen.elements [ Lt; Le; Gt; Ge; Eq; Ne ]

let private genIntConst = Gen.frequency [ 3, Gen.elements [ 0; 1; -1; 2; 10 ]; 1, Gen.choose (-100, 100); 1, Gen.elements [ Int32.MaxValue; Int32.MinValue ] ]

let private genDoubleConst =
    Gen.frequency
        [ 3, Gen.elements [ 0.0; 1.0; -1.0; 2.5; -0.0 ]
          2, Gen.elements [ Double.NaN; Double.PositiveInfinity; Double.NegativeInfinity ]
          // constants whose sums and products round or overflow
          2, Gen.elements [ 0.1; 0.2; 10.0; 1e16; 1e308 ]
          1, Gen.choose (-1000, 1000) |> Gen.map (fun n -> float n / 4.0) ]

/// Zeros and ones of several scales, and the ends of the range (their sums overflow).
let private genDecimalConst =
    Gen.frequency
        [ 4, Gen.elements [ 0m; 1m; -1m; 0.00m; 1.0m; 1.50m; 2m ]
          1, Gen.elements [ Decimal.MaxValue; Decimal.MinValue ]
          1, Gen.choose (-1000, 1000) |> Gen.map (fun n -> decimal n / 8m) ]

let private genStringConst = Gen.elements [ ""; "a"; "ab"; "b"; " "; null ]

let rec genB (depth: int) : Gen<BExpr> =
    let leaf =
        Gen.frequency
            [ 4, Gen.choose (0, 2) |> Gen.map BVar
              2, Gen.elements [ BConst true; BConst false ]
              2, Gen.elements [ OFlag; OIsNull; ONotNull ] ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let i = genI (depth - 1)
        let d = genD (depth - 1)
        let n = genN (depth - 1)
        let s = genS (depth - 1)
        let m = genM (depth - 1)
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
              1, Gen.zip3 genCmp m m |> Gen.map MCmp
              1, Gen.zip s s |> Gen.map SEq
              1, Gen.map SEmpty s
              1, Gen.map NHasValue n
              1, Gen.zip3 b b b |> Gen.map BIf
              6, genShaped (depth - 1) ]

and genI (depth: int) : Gen<IExpr> =
    let leaf =
        Gen.frequency
            [ 5, Gen.choose (0, 4) |> Gen.map IVar
              3, Gen.map IConst genIntConst
              1, Gen.map Captured genIntConst
              1, Gen.constant OValue ]
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
              1, Gen.zip d d |> Gen.map DSub
              2, Gen.zip d d |> Gen.map DMul
              1, Gen.zip d d |> Gen.map DDiv
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
        Gen.frequency
            [ 4, leaf
              2, Gen.zip n n |> Gen.map NAdd
              1, Gen.zip n n |> Gen.map NSub
              2, Gen.zip n n |> Gen.map NMul
              1, Gen.zip n n |> Gen.map NMod
              1, Gen.zip3 b n n |> Gen.map NIf ]

and genM (depth: int) : Gen<MExpr> =
    let leaf = Gen.frequency [ 3, Gen.constant MVar; 3, Gen.map MConst genDecimalConst ]
    if depth <= 0 then
        leaf
    else
        let b = genB (depth - 1)
        let m = genM (depth - 1)
        Gen.frequency
            [ 4, leaf
              2, Gen.zip m m |> Gen.map MAdd
              2, Gen.zip m m |> Gen.map MSub
              2, Gen.zip m m |> Gen.map MMul
              1, Gen.zip m m |> Gen.map MDiv
              1, Gen.zip3 b m m |> Gen.map MIf
              // the identities and folds, over zeros and ones of several scales
              3, gen {
                    let! zero = Gen.elements [ 0m; 0.00m ]
                    let! one = Gen.elements [ 1m; 1.0m; 1.00m ]
                    let! k1 = genDecimalConst
                    let! k2 = genDecimalConst
                    let! x = m
                    return!
                        Gen.elements
                            [ MAdd (x, MConst zero); MAdd (MConst zero, x); MSub (x, MConst zero)
                              MMul (x, MConst zero); MMul (MConst zero, x)
                              MMul (x, MConst one); MMul (MConst one, x); MDiv (x, MConst one)
                              MSub (MVar, MVar)
                              MAdd (MConst k1, MConst k2); MSub (MConst k1, MConst k2)
                              MMul (MConst k1, MConst k2); MDiv (MConst k1, MConst k2) ]
                 } ]

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
/// comparison ranges on one variable (x > 1 and x > 2, also over a double
/// with NaN limits and over nullables), duplicated and mutually exclusive
/// comparisons, the same shapes over two parameters that only share a name,
/// reads of an object behind its null guard, and and/or chains of 8 to 40
/// operands for balancing.
and genShaped (depth: int) : Gen<BExpr> =
    let b = genB (max 0 depth)
    let variable = Gen.choose (0, 4) |> Gen.map IVar
    let join = Gen.elements [ AndAlso; OrElse ]
    let doubleRange =
        gen {
            let! op1 = genCmp
            let! op2 = genCmp
            let! k1 = genDoubleConst
            let! k2 = genDoubleConst
            let! join = join
            return join (DCmp (op1, DVar, DConst k1), DCmp (op2, DVar, DConst k2))
        }
    let nullableRange =
        gen {
            let! op1 = genCmp
            let! op2 = genCmp
            let limit = Gen.frequency [ 3, Gen.map (Some >> NConst) genIntConst; 1, Gen.constant (NConst None); 2, Gen.constant (NVar 1) ]
            let! k1 = limit
            let! k2 = limit
            let! join = join
            return join (NCmp (op1, NVar 0, k1), NCmp (op2, NVar 0, k2))
        }
    /// x and the other parameter named x (or the unnamed one) where a rule wants one operand twice
    let namesakes =
        gen {
            let! other = Gen.elements [ IVar 3; IVar 4 ]
            let! op1 = genCmp
            let! op2 = genCmp
            let! k1 = genIntConst
            let! k2 = genIntConst
            let! join = join
            return!
                Gen.elements
                    [ ICmp (op1, IVar 0, other)
                      ICmp (op1, Sub (IVar 0, other), IConst k1)
                      join (ICmp (op1, IVar 0, IConst k1), ICmp (op2, other, IConst k2))
                      join (ICmp (op1, IVar 0, IConst k1), ICmp (op1, other, IConst k1)) ]
        }
    /// o.Flag and o.Value read behind o != null (or after o == null), in the shapes the rules regroup
    let guarded =
        gen {
            let! x = b
            let read = ICmp (Gt, OValue, IConst 0)
            return!
                Gen.elements
                    [ AndAlso (AndAlso (ONotNull, OrElse (OFlag, x)), OFlag)
                      AndAlso (AndAlso (OrElse (x, OFlag), ONotNull), OFlag)
                      OrElse (AndAlso (x, OFlag), OrElse (OIsNull, OFlag))
                      OrElse (OrElse (AndAlso (x, OFlag), OIsNull), OFlag)
                      OrElse (Not (OrElse (x, ONotNull)), OrElse (x, read))
                      OrElse (OrElse (Not x, OIsNull), AndAlso (read, Not OIsNull))
                      OrElse (Not (OrElse (x, OIsNull)), AndAlso (Not OIsNull, read))
                      AndAlso (ONotNull, OrElse (x, AndAlso (OFlag, read)))
                      OrElse (OIsNull, AndAlso (OFlag, x)) ]
        }
    let range =
        gen {
            let! v = variable
            let! op1 = genCmp
            let! op2 = genCmp
            let! k1 = genIntConst
            let! k2 = genIntConst
            let! join = join
            return join (ICmp (op1, v, IConst k1), ICmp (op2, v, IConst k2))
        }
    let exclusive =
        gen {
            let! v = variable
            let! k = genIntConst
            let! op1, op2 = Gen.elements [ Eq, Ne; Ne, Eq; Lt, Ge; Gt, Le; Ge, Lt; Le, Gt; Ge, Le ]
            let! join = join
            return!
                Gen.elements
                    [ join (ICmp (op1, v, IConst k), ICmp (op2, v, IConst k))
                      join (NCmp (op1, NVar 0, NVar 1), NCmp (op2, NVar 0, NVar 1))
                      join (DCmp (op1, DVar, DConst (float k)), DCmp (op2, DVar, DConst (float k))) ]
        }
    let chain =
        gen {
            let! length = Gen.choose (8, 40)
            let! leaves = Gen.listOfLength length (genB 0)
            let! join = join
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
          // an operand absorbed two links down, on either side
          2, Gen.zip3 (genB 0) b b |> Gen.map (fun (a, x, y) -> AndAlso (a, AndAlso (x, OrElse (a, y))))
          2, Gen.zip3 (genB 0) b b |> Gen.map (fun (a, x, y) -> OrElse (OrElse (x, AndAlso (y, a)), a))
          // a complement pair factored out of one operand
          2, Gen.zip (genB 0) (genB 0) |> Gen.map (fun (a, x) -> OrElse (AndAlso (a, x), AndAlso (Not a, x)))
          // an operand beside its complement one link down
          2, Gen.zip (genB 0) b |> Gen.map (fun (a, x) -> AndAlso (a, OrElse (Not a, x)))
          2, Gen.zip (genB 0) b |> Gen.map (fun (a, x) -> OrElse (AndAlso (x, Not a), a))
          4, range
          2, doubleRange
          2, nullableRange
          3, exclusive
          2, namesakes
          4, guarded
          2, chain ]
let genTree : Gen<Tree> =
    Gen.sized (fun size ->
        let depth = min 5 (1 + size / 15)
        Gen.frequency
            [ 6, Gen.map TB (genB depth)
              4, Gen.map TI (genI depth)
              1, Gen.map TD (genD depth)
              1, Gen.map TN (genN depth)
              1, Gen.map TS (genS depth)
              1, Gen.map TM (genM depth) ])

// ============================================================================
// Shrinking: a node shrinks to any of its children of the same type
// ============================================================================

let rec shrinkB (e: BExpr) : seq<BExpr> =
    seq {
        match e with
        | BVar _ -> ()
        | BConst _ -> ()
        | OFlag | OIsNull | ONotNull -> ()
        | Not a -> yield a; yield! shrinkB a |> Seq.map Not
        | AndAlso (a, b) -> yield a; yield b; yield! shrinkB a |> Seq.map (fun a' -> AndAlso (a', b)); yield! shrinkB b |> Seq.map (fun b' -> AndAlso (a, b'))
        | OrElse (a, b) -> yield a; yield b; yield! shrinkB a |> Seq.map (fun a' -> OrElse (a', b)); yield! shrinkB b |> Seq.map (fun b' -> OrElse (a, b'))
        | BAnd (a, b) | BOr (a, b) | BXor (a, b) | BEq (a, b) | BNe (a, b) -> yield a; yield b
        | ICmp (c, a, b) -> yield! shrinkI a |> Seq.map (fun a' -> ICmp (c, a', b)); yield! shrinkI b |> Seq.map (fun b' -> ICmp (c, a, b'))
        | DCmp (c, a, b) -> yield! shrinkD a |> Seq.map (fun a' -> DCmp (c, a', b)); yield! shrinkD b |> Seq.map (fun b' -> DCmp (c, a, b'))
        | NCmp (c, a, b) -> yield! shrinkN a |> Seq.map (fun a' -> NCmp (c, a', b)); yield! shrinkN b |> Seq.map (fun b' -> NCmp (c, a, b'))
        | MCmp (c, a, b) -> yield! shrinkM a |> Seq.map (fun a' -> MCmp (c, a', b)); yield! shrinkM b |> Seq.map (fun b' -> MCmp (c, a, b'))
        | SEq (a, b) -> yield! shrinkS a |> Seq.map (fun a' -> SEq (a', b)); yield! shrinkS b |> Seq.map (fun b' -> SEq (a, b'))
        | SEmpty a -> yield! shrinkS a |> Seq.map SEmpty
        | NHasValue a -> yield! shrinkN a |> Seq.map NHasValue
        | BIf (c, a, b) -> yield c; yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> BIf (c', a, b))
    }

and shrinkI (e: IExpr) : seq<IExpr> =
    seq {
        match e with
        | IVar _ | IConst _ | Captured _ | OValue -> ()
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
        | DAdd (a, b) | DSub (a, b) | DMul (a, b) | DDiv (a, b) -> yield a; yield b
        | DIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> DIf (c', a, b))
    }

and shrinkN (e: NExpr) : seq<NExpr> =
    seq {
        match e with
        | NVar _ | NConst _ -> ()
        | NAdd (a, b) | NSub (a, b) | NMul (a, b) | NMod (a, b) -> yield a; yield b
        | NIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> NIf (c', a, b))
    }

and shrinkM (e: MExpr) : seq<MExpr> =
    seq {
        match e with
        | MVar | MConst _ -> ()
        | MAdd (a, b) | MSub (a, b) | MMul (a, b) | MDiv (a, b) -> yield a; yield b
        | MIf (c, a, b) -> yield a; yield b; yield! shrinkB c |> Seq.map (fun c' -> MIf (c', a, b))
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
    | TM e -> shrinkM e |> Seq.map TM

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
        let! x2 = genIntConst
        let! u = genIntConst
        let! v = Gen.elements [ true; false ]
        let! m = genDecimalConst
        let! o = Gen.elements [ null; Holder 1; Holder 0 ]
        return { X = x; Y = y; Z = z; P = p; Q = q; S = s; T = t; NX = nx; NY = ny; D = d; X2 = x2; U = u; V = v; M = m; O = o }
    }

/// A fixed grid of environments every tree is checked on, so a failure is
/// reproducible from the tree alone.
let envs : Env list =
    let ints = [ 0; 1; -1; 2; Int32.MaxValue ]
    let strings = [ ""; "a"; null ]
    let nullables = [ Nullable<int>(); Nullable 0; Nullable 1 ]
    let doubles = [ 0.0; 1.0; Double.NaN; -0.0; 1e16; 1e308 ]
    let decimals = [ 0m; 1.50m; -1m; Decimal.MaxValue ]
    let holders = [ null; Holder 1; Holder 0 ]
    [ for i in 0 .. 71 ->
          { X = ints.[i % 5]
            Y = ints.[(i / 5) % 5]
            Z = ints.[(i / 25) % 5]
            P = i % 2 = 0
            Q = (i / 2) % 2 = 0
            S = strings.[i % 3]
            T = strings.[(i / 3) % 3]
            NX = nullables.[(i / 7) % 3]
            NY = nullables.[(i / 11) % 3]
            D = doubles.[(i / 4) % 6]
            X2 = ints.[(i / 2) % 5]
            U = ints.[(i / 3) % 5]
            V = (i / 3) % 2 = 0
            M = decimals.[(i / 2) % 4]
            O = holders.[(i / 2) % 3] } ]
