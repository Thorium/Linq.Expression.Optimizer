// --------------------------------------------------------------------------------------
// FAKE 6 build script
// --------------------------------------------------------------------------------------
// Run with: dotnet fake run build.fsx
// Or via:   build.cmd / build.sh

#r "paket:
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Paket
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git
nuget Fake.Api.GitHub //"

#if !FAKE
#load "./.fake/build.fsx/intellisense.fsx"
#endif

open System
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

// Initialize FAKE context from command-line arguments
Target.initEnvironment()

// --------------------------------------------------------------------------------------
// Project information

let project = "Linq.Expression.Optimizer"

let summary = "Lightweight optimizer of System.Linq.Expression expressions. Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations."

let description = "Lightweight optimizer of System.Linq.Expression expressions. Just basic boolean algebra and reductions, constant and tuple/anonymous type eliminations. For side-effect free Expressions. No compilation-subjective optimizations. This is meant to be used with expressions that are not compiled but transferred to other domain."

let authors = [ "Tuomas Hietanen" ]

let tags = "LINQ, Expression, Expressions, Expression tree, expressiontree, expression-tree, IQueryable, System.Linq.Expressions, optimise, optimize, compile, boolean, algebra, simplification, deMorgan, reduction, optimizer, optimiser, tree"

let solutionFile = "Linq.Expression.Optimizer.sln"

let mainProject = "src" </> "Linq.Expression.Optimizer" </> "Linq.Expression.Optimizer.fsproj"

// Git configuration
let gitOwner = "Thorium"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner
let gitName = "Linq.Expression.Optimizer"
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/Thorium"

// Read version and release notes from RELEASE_NOTES.md
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// MSBuild properties for versioning
let versionProps = [
    "Version", release.NugetVersion
    "AssemblyVersion", release.AssemblyVersion
    "FileVersion", release.AssemblyVersion
    "InformationalVersion", release.NugetVersion
]

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    DotNet.build (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            MSBuildParams = { p.MSBuildParams with Properties = versionProps }
        }
    ) solutionFile
)

// --------------------------------------------------------------------------------------
// Run the unit tests

Target.create "RunTests" (fun _ ->
    DotNet.test (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            NoBuild = true
        }
    ) solutionFile
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            NoBuild = true
            OutputPath = Some "bin"
            MSBuildParams = {
                p.MSBuildParams with
                    Properties = versionProps @ [
                        "PackageReleaseNotes", (release.Notes |> String.concat "\n")
                    ]
            }
        }
    ) mainProject
)

Target.create "PublishNuget" (fun _ ->
    let nupkg =
        !! "bin/Linq.Expression.Optimizer.*.nupkg"
        |> Seq.head
    let result =
        DotNet.exec id "nuget"
            (sprintf "push \"%s\" --source https://api.nuget.org/v3/index.json" nupkg)
    if not result.OK then failwithf "nuget push failed for %s" nupkg
)

// --------------------------------------------------------------------------------------
// Alternate: use paket pack/push (requires dotnet paket tool)
// Uncomment these targets and comment out NuGet/PublishNuget above if you prefer paket.

// Target.create "PaketPack" (fun _ ->
//     let result =
//         DotNet.exec id "paket"
//             (sprintf "pack bin --version %s" release.NugetVersion)
//     if not result.OK then failwith "paket pack failed"
// )
//
// Target.create "PaketPush" (fun _ ->
//     let result = DotNet.exec id "paket" "push bin"
//     if not result.OK then failwith "paket push failed"
// )

// --------------------------------------------------------------------------------------
// Aggregate targets

Target.create "BuildPackage" ignore
Target.create "All" ignore

// --------------------------------------------------------------------------------------
// Target dependencies

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "NuGet"
  ==> "BuildPackage"
  ==> "All"

"BuildPackage"
  ==> "PublishNuget"

Target.runOrDefaultWithArguments "All"
