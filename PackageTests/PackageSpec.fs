module SafetyFirst.PackageSpecs.PackageSpec

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Text

open NUnit.Framework
open Swensen.Unquote

open SafetyFirst
open SafetyFirst.Numbers

let packageAssembly = typeof<NaturalInt>.Assembly

let sourceLinkKind = Guid "CC110556-A091-4D38-9FEC-25AB9A351A6A"

[<Test>]
let ``referenced SafetyFirst is the version just packed`` () =
  let expected =
    Assembly.GetExecutingAssembly().GetCustomAttributes<AssemblyMetadataAttribute>()
    |> Seq.find (fun a -> a.Key = "SafetyFirstVersion")
    |> _.Value

  let actual =
    packageAssembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>().InformationalVersion

  // INFORMATIONAL VERSION CARRIES A "+<commit>" SUFFIX.
  test <@ actual.Split('+').[0] = expected @>

[<Test>]
let ``total functions work through the packed library`` () =
  test <@ Seq.head' [ 1; 2; 3 ] = Ok 1 @>
  test <@ List.NonEmpty.max (List.NonEmpty.create 0 [ 1..10 ]) = 10 @>

// and! APPENDS ERRORS THROUGH FSharpPlus, SO THIS ALSO LOADS THE PACKAGE'S DEPENDENCIES.
[<Test>]
let ``applicative result aggregates errors`` () =
  let actual =
    result {
      let! x = Error [ "no x value available" ]
      and! y = Error [ "no y value available" ]
      return x + y
    }

  test <@ actual = Error [ "no x value available"; "no y value available" ] @>

let readEmbeddedPdb () =
  use pe = new PEReader(File.OpenRead packageAssembly.Location)

  pe.ReadDebugDirectory()
  |> Seq.tryFind (fun e -> e.Type = DebugDirectoryEntryType.EmbeddedPortablePdb)
  |> Option.map pe.ReadEmbeddedPortablePdbDebugDirectoryData

[<Test>]
let ``DLL embeds a PDB with Source Link to GitHub`` () =
  let pdbProvider = readEmbeddedPdb ()
  test <@ pdbProvider.IsSome @>
  use pdbProvider = pdbProvider.Value
  let pdb = pdbProvider.GetMetadataReader()

  let sourceLink =
    pdb.CustomDebugInformation
    |> Seq.map pdb.GetCustomDebugInformation
    |> Seq.tryFind (fun cdi -> pdb.GetGuid cdi.Kind = sourceLinkKind)
    |> Option.map (fun cdi -> Encoding.UTF8.GetString(pdb.GetBlobBytes cdi.Value))

  test
    <@
      sourceLink
      |> Option.exists _.Contains("https://raw.githubusercontent.com/ntwilson/SafetyFirst/")
    @>

// A REAL PATH WOULD LET EDITORS ON THE BUILD MACHINE OPEN LOCAL FILES, MASKING BROKEN SOURCE LINK.
[<Test>]
let ``PDB records only deterministic source paths`` () =
  let pdbProvider = readEmbeddedPdb ()
  test <@ pdbProvider.IsSome @>
  use pdbProvider = pdbProvider.Value
  let pdb = pdbProvider.GetMetadataReader()

  let localPaths =
    pdb.Documents
    |> Seq.map (fun h -> pdb.GetString (pdb.GetDocument h).Name)
    |> Seq.filter (fun path -> not (path.StartsWith "/_"))
    |> Seq.toList

  test <@ localPaths = [] @>
