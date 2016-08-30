namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Linq.Expression.Optimizer")>]
[<assembly: AssemblyProductAttribute("Linq.Expression.Optimizer")>]
[<assembly: AssemblyDescriptionAttribute("Lightweight optimizer of System.Linq.Expression expressions. Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations.")>]
[<assembly: AssemblyVersionAttribute("1.0.2")>]
[<assembly: AssemblyFileVersionAttribute("1.0.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.2"
    let [<Literal>] InformationalVersion = "1.0.2"
