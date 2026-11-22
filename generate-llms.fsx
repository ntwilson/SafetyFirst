// Rewrites the generated API index in llms.txt from the compiler's signature file.
// BUILD SafetyFirst (netstandard2.0) FIRST: THE BUILD IS WHAT WRITES SafetyFirst/obj/SafetyFirst.fsi.

open System
open System.IO
open System.Text.RegularExpressions

let root = __SOURCE_DIRECTORY__
let signaturePath = Path.Combine (root, "SafetyFirst", "obj", "SafetyFirst.fsi")
let llmsPath = Path.Combine (root, "llms.txt")

let beginMarker = "<!-- BEGIN GENERATED API INDEX (generate-llms.fsx) -->"
let endMarker = "<!-- END GENERATED API INDEX -->"

type ModuleScope = { Indent : int; Name : string; Hidden : bool; AutoOpen : bool }

type Declaration = { Module : string; AutoOpen : bool; Name : string }

let indentOf (line : string) = line.Length - line.TrimStart().Length

let isAnnotation (line : string) =
  let t = line.TrimStart ()
  t.StartsWith "///" || t.StartsWith "[<"

/// Splits on blank lines; the compiler separates every declaration with one.
let blocks (lines : string list) =
  let close current acc = if List.isEmpty current then acc else List.rev current :: acc
  let current, acc =
    lines |> List.fold (fun (current, acc) line ->
      if String.IsNullOrWhiteSpace line then [], close current acc
      else line :: current, acc) ([], [])
  close current acc |> List.rev

let namespaceRegex = Regex @"^namespace\s+([\w.]+)\s*$"
let moduleRegex = Regex @"^(\s*)module\s+(?:(private|internal)\s+)?([\w.']+)(\s*=)?\s*$"
let valRegex = Regex @"^(\s*)val\s+((?:(?:inline|private|internal|mutable)\s+)*)(\(.*?\)|[\w']+)"

let qualifiedName (scopes : ModuleScope list) =
  let full = scopes |> List.rev |> List.map (fun s -> s.Name) |> String.concat "."
  if full.StartsWith "SafetyFirst." then full.Substring "SafetyFirst.".Length else full

/// Folds over blocks, tracking the enclosing modules by indentation.
let declarations (lines : string list) =
  let step (scopes : ModuleScope list, found : Declaration list) (block : string list) =
    match block |> List.skipWhile isAnnotation with
    | [] -> scopes, found
    | head :: _ ->
      let inScope = scopes |> List.filter (fun s -> s.Indent < indentOf head)
      let ns = namespaceRegex.Match head
      let m = moduleRegex.Match head
      let v = valRegex.Match head
      if ns.Success then
        [ { Indent = -1; Name = ns.Groups.[1].Value; Hidden = false; AutoOpen = false } ], found
      elif m.Success then
        let nested = m.Groups.[4].Success
        let scope =
          { Indent = (if nested then m.Groups.[1].Length else -1)
            Name = m.Groups.[3].Value
            Hidden = m.Groups.[2].Success
            AutoOpen = block |> List.exists (fun l -> l.Contains "[<AutoOpen>]") }
        // A TOP-LEVEL `module A.B` (NO '=') REPLACES THE SCOPE INSTEAD OF NESTING.
        (if nested then scope :: inScope else [ scope ]), found
      elif v.Success then
        let modifiers = v.Groups.[2].Value
        let hidden =
          modifiers.Contains "private" || modifiers.Contains "internal" || inScope |> List.exists (fun s -> s.Hidden)
        let decl =
          { Module = qualifiedName inScope
            AutoOpen = inScope |> List.tryHead |> Option.exists (fun s -> s.AutoOpen)
            Name = v.Groups.[3].Value }
        inScope, (if hidden then found else decl :: found)
      else inScope, found
  lines |> blocks |> List.fold step ([], []) |> snd |> List.rev

type Variant = Safe | Prime | Plain | Try

/// `fooSafe`, `foo'` and `tryFoo` all share the base name `foo`.
let classify (name : string) =
  if name.Length > 4 && name.EndsWith "Safe" then name.Substring (0, name.Length - 4), Safe
  elif name.Length > 1 && name.EndsWith "'" then name.TrimEnd '\'', Prime
  elif name.Length > 3 && name.StartsWith "try" && Char.IsUpper name.[3] then
    string (Char.ToLowerInvariant name.[3]) + name.Substring 4, Try
  else name, Plain

/// One entry per base name, e.g. "headSafe/head'/tryHead". Overloads collapse to one name.
let entries (decls : Declaration list) =
  decls
  |> List.map (fun d -> d.Name)
  |> List.distinct
  |> List.groupBy (classify >> fst)
  |> List.map (fun (_, names) -> names |> List.sortBy (classify >> snd) |> String.concat "/")

/// Greedily packs entries into lines of at most `width` characters.
let wrap width (items : string list) =
  let lines =
    items |> List.fold (fun lines item ->
      match lines with
      | current :: rest when (current : string).Length + 2 + item.Length <= width -> $"%s{current}, %s{item}" :: rest
      | _ -> item :: lines) []
  List.rev lines

let renderIndex (decls : Declaration list) =
  let modules =
    decls
    |> List.groupBy (fun d -> d.Module)
    |> List.map (fun (name, ds) ->
      let title = if ds |> List.exists (fun d -> d.AutoOpen) then $"%s{name} (AutoOpen)" else name
      title, entries ds)
  let listing items = [ yield "```"; yield! wrap 100 items; yield "```" ]
  modules
  |> List.mapi (fun i (title, items) ->
    // ALIAS MODULES (e.g. FSeq FOR FiniteSeq) WOULD OTHERWISE REPEAT THEIR WHOLE LISTING.
    // SMALL MODULES ARE LISTED IN FULL: A POINTER TO A 2-NAME MODULE ELSEWHERE SAVES NOTHING.
    let mine = Set.ofList items
    let twin =
      modules
      |> List.take i
      |> List.filter (fun _ -> mine.Count >= 10)
      |> List.map (fun (other, theirs) -> other, Set.difference mine (Set.ofList theirs), Set.difference (Set.ofList theirs) mine)
      |> List.filter (fun (_, extra, missing) -> extra.Count + missing.Count <= 3)
      |> List.sortBy (fun (_, extra, missing) -> extra.Count + missing.Count)
      |> List.tryHead
    let body =
      match twin with
      | Some (other, extra, missing) ->
        let names (s : Set<string>) = s |> Set.toList |> String.concat ", "
        [ yield $"Same functions as `%s{other}`."
          if not extra.IsEmpty then yield $"Also has: %s{names extra}."
          if not missing.IsEmpty then yield $"Lacks: %s{names missing}." ]
        |> String.concat " "
        |> List.singleton
      | None -> listing items
    [ ""; $"### %s{title}"; "" ] @ body)
  |> List.concat

let splice (newline : string) (generated : string list) (existing : string) =
  let start = existing.IndexOf beginMarker
  let finish = existing.IndexOf endMarker
  if start < 0 || finish < start then
    Error $"llms.txt must contain the markers %s{beginMarker} and %s{endMarker}, in that order"
  else
    let before = existing.Substring (0, start + beginMarker.Length)
    let after = existing.Substring finish
    Ok (before + newline + String.concat newline generated + newline + newline + after)

let run () =
  if not (File.Exists signaturePath) then
    Error $"%s{signaturePath} not found. Build SafetyFirst first (dotnet build SafetyFirst -f netstandard2.0)."
  else
    let existing = File.ReadAllText llmsPath
    let newline = if existing.Contains "\r\n" then "\r\n" else "\n"
    let generated = File.ReadAllLines signaturePath |> List.ofArray |> declarations |> renderIndex
    splice newline generated existing
    |> Result.map (fun text -> File.WriteAllText (llmsPath, text))

match run () with
| Ok () -> printfn "Updated %s" llmsPath
| Error message ->
  eprintfn "%s" message
  exit 1
