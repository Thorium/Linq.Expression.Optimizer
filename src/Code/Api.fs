namespace System.Linq
[<System.Runtime.CompilerServices.Extension>]
type OptimizeExtension =
    /// Expression tree visit'or: go through the whole expression tree.
    [<System.Runtime.CompilerServices.Extension>]
    static member Optimize(exp:System.Linq.Expressions.Expression) = 
        ExpressionOptimizer.visit' exp

    [<System.Runtime.CompilerServices.Extension>]
    static member Optimize<'T>(exp:System.Linq.Expressions.Expression<'T>) = 
        ExpressionOptimizer.visit' exp :?> System.Linq.Expressions.Expression<'T>
