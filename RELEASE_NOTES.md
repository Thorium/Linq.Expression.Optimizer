### 1.0.34 - 09/11/2025
* A factorComplement rule to address #18

### 1.0.33 - 18/09/2025
* A few new aritmethics rules
* Minor performance improvements

### 1.0.32 - 19/06/2025
* Known null.hasValue is always false
* X times zero equals zero

### 1.0.31 - 04/06/2025
* FSharp.Core dependency update
* NET8.0 target added

### 1.0.29 - 31/03/2025
* Further Improved boolean algebra optimisation
* Initial implementation of "remove duplicate condition", "remove mutually exclusive condition"

### 1.0.28 - 26/03/2025
* Improved boolean algebra optimisation

### 1.0.27 - 18/03/2025
* Added propertyMatch to ensure a.X = a.X even when not same memory instance of a.
* Added some rules to overcome issues with associate and distribute loops.

### 1.0.24 - 07/11/2024
* Perf opt: Faster type tests

### 1.0.23 - 05/04/2024
* Typed visit for C# wrapper, fix #13
* Minor performance optimisation

### 1.0.21 - 05/04/2024
* Minor performance optimisation

### 1.0.20 - 02/04/2024
* FSharp.Core update
* Minor performance updates

### 1.0.18 - 11/03/2024
* Support for basic math optimisation: ((x+2)+2)=x+4, #16

### 1.0.16 - 08/10/2023
* Minor performance optimisations
* Relaxed F# dependency
* Dropped support for old .NET 3.5

### 1.0.15 - 07/03/2023
* Dependency updates
* Fix for ExpressionType.Extension

### 1.0.14 - 28/10/2020
* FSharp core dependency update

### 1.0.13 - 27/05/2019
* Look inside a WhereSelectEnumerableIterator, #9

### 1.0.12 - 31/08/2018
* Reference updates
* Null-value handling on ``replace constant comparison``, #7

### 1.0.11 - 01/01/2018
* Updated FSharp from 4.2 to 4.2.3
* Renamed netstandard1.6 dll to be consistent with others
* Updated to build with .NET Standard 2 SDK

### 1.0.9 - 05/06/2017
* Changed netcoreapp1.1 to netstandard1.6

### 1.0.8 - 10/05/2017
* One more optimization: Simple constant math evaluation

### 1.0.7 - 07/10/2016
* C# Api

### 1.0.6 - 06/10/2016
* Improved compatibility with C# LINQ projects.

### 1.0.5 - 05/10/2016
* Minor performance tweaks
* Better C# LINQ optimization

### 1.0.4 - 30/08/2016
* Minor tweaks to optimization

### 1.0.3 - 30/08/2016
* Constant evaluation.

### 1.0.2 - 30/08/2016
* Better support of C# anonymous types

### 1.0.1 - 30/08/2016
* Support for: net35/net45/netcore

### 1.0 - 27/08/2016
* First version
