let ( let* ) = Result.bind
let max_results = 200

type error = Invalid_pattern of string | Glob_error of string

let pp_error = function
  | Invalid_pattern msg -> Fmt.str "invalid pattern: %s" msg
  | Glob_error msg -> msg

let err_glob_error fmt = Fmt.kstr (fun s -> Error (Glob_error s)) fmt
let err_invalid_pattern fmt = Fmt.kstr (fun s -> Error (Invalid_pattern s)) fmt

(* Directories to skip during traversal: hidden dirs and common build
   artifacts. Deliberately a fixed list rather than parsing .ignore/.gitignore:
   it keeps op minimal and, more importantly, uniform — the other tools (nu,
   read) are not ignore-aware either, so an ignore-file parser here would make
   glob the lone exception. This mirrors ripgrep's default skips and is enough
   to keep traversal fast and quiet. *)
let should_skip_dir name =
  (String.length name > 0 && name.[0] = '.')
  || List.mem name [ "_build"; "_opam"; "node_modules" ]

(* Symlinked directories are treated as files rather than followed, so
   cyclic links cannot loop the walk; broken links and unstatable entries
   also count as files. *)
let is_real_dir full =
  match Unix.lstat full with
  | { Unix.st_kind = Unix.S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error _ -> false

(* Recursively walk a directory tree, collecting file paths relative to the
   search root. [prefix] is the relative path accumulated so far (empty at
   the root). *)
let rec walk acc dir prefix =
  let entries = try Sys.readdir dir with Sys_error _ -> [||] in
  Array.fold_left
    (fun acc name ->
      let full = Filename.concat dir name in
      let rel = if prefix = "" then name else prefix ^ "/" ^ name in
      if is_real_dir full then
        if should_skip_dir name then acc else walk acc full rel
      else rel :: acc)
    acc entries

(* Bash globstar lets '**/' match zero directories: '**/*.ml' matches a
   top-level 'a.ml' and 'lib/**/x' matches 'lib/x'. Re.Glob's
   [double_asterisk] makes '**' match slashes but never the empty component,
   so expand the pattern into the set of variants with each
   component-position '**/' elided and match any of them. *)
let zero_dir_variants pattern =
  let elisions pat =
    let n = String.length pat in
    let rec collect i acc =
      if i + 3 > n then acc
      else if String.sub pat i 3 = "**/" && (i = 0 || pat.[i - 1] = '/') then
        collect (i + 1)
          ((String.sub pat 0 i ^ String.sub pat (i + 3) (n - i - 3)) :: acc)
      else collect (i + 1) acc
    in
    collect 0 []
  in
  let rec expand seen = function
    | [] -> List.rev seen
    | pat :: rest ->
        if List.mem pat seen then expand seen rest
        else expand (pat :: seen) (elisions pat @ rest)
  in
  expand [] [ pattern ]

(* [zero_dir_variants] doubles the variant set for every component-position
   '**/', so it is exponential in their count. Realistic patterns have one or
   two; refuse absurd ones up front to keep the expansion formally bounded
   (2^max_globstars variants). *)
let max_globstars = 8

let count_globstars pattern =
  let n = String.length pattern in
  let rec count i acc =
    if i + 3 > n then acc
    else if String.sub pattern i 3 = "**/" && (i = 0 || pattern.[i - 1] = '/')
    then count (i + 1) (acc + 1)
    else count (i + 1) acc
  in
  count 0 0

(* Anchored so a pattern must match the whole relative path: unanchored,
   '*.ml' would find a match inside 'sub/nested.ml' and recurse by accident. *)
let compile pattern =
  if count_globstars pattern > max_globstars then
    err_invalid_pattern "%s (too many '**/' segments; max %d)" pattern
      max_globstars
  else
    let rec compile_all acc = function
      | [] -> Ok (Re.compile (Re.alt (List.rev acc)))
      | v :: rest -> (
          match Re.Glob.glob_result ~anchored:true ~expand_braces:true v with
          | Error `Parse_error -> Error (Invalid_pattern pattern)
          | Ok re -> compile_all (re :: acc) rest)
    in
    compile_all [] (zero_dir_variants pattern)

(* Expand a glob pattern relative to an optional path. The pattern is matched
   against paths relative to the search root; results are then prefixed with
   the path to make them relative to CWD. This means:

   - glob ~pattern:"*.ml"           → only top-level .ml files
   - glob ~pattern:"**/*.ml"        → .ml files at any depth
   - glob ~pattern:"*.ml" ~path:"lib" → .ml files directly under lib/
   - glob ~pattern:"**/*.ml" ~path:"lib" → .ml files anywhere under lib/

   Results are sorted and capped at [max_results]. *)
let run ~pattern ?(path = ".") () =
  if not (try Sys.is_directory path with Sys_error _ -> false) then
    err_glob_error "no such directory: %s" path
  else
    let* re = compile pattern in
    let files = List.sort String.compare (walk [] path "") in
    let matched = List.filter (fun name -> Re.execp re name) files in
    let prefix =
      if path = "." then ""
      else if String.length path > 0 && path.[String.length path - 1] = '/' then
        path
      else path ^ "/"
    in
    let total = List.length matched in
    let capped = if total > max_results then max_results else total in
    let lines = List.filteri (fun i _ -> i < capped) matched in
    let buf = Buffer.create 256 in
    List.iter
      (fun p ->
        Buffer.add_string buf prefix;
        Buffer.add_string buf p;
        Buffer.add_char buf '\n')
      lines;
    if total > capped then
      Fmt.pf (Fmt.with_buffer buf) "... (%d more results)@." (total - capped);
    Ok (Buffer.contents buf)

let tool : Tool.t =
  {
    name = "glob";
    description =
      "List files matching a glob pattern. Returns sorted file paths relative \
       to the working directory. Use this to discover files instead of running \
       ls/find in a script. Patterns use shell glob syntax: * matches any \
       non-slash characters, ** matches across directories, ? matches a single \
       character, {a,b} alternates.";
    parameters =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pattern",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Glob pattern (e.g. *.ml, **/*.ml, {lib,bin}/*.ml)" );
                    ] );
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Directory to search in (defaults to current \
                           directory)" );
                    ] );
              ] );
          ("required", `List [ `String "pattern" ]);
        ];
    run =
      (fun args ->
        let* pattern = Tool.required_string args "pattern" in
        let path = Tool.optional_string args "path" in
        match run ~pattern ?path () with
        | Ok content -> Tool.text content
        | Error e -> Error (pp_error e));
  }
