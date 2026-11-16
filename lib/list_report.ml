(** Pure JSON renderers for [op --list --json].

    Each function returns a single compact JSON line for stdout. Empty results
    are a success with an empty array — never an error; the caller signals
    failures with {!error}. [bin/main.ml] owns data fetching, the human-readable
    text path, stderr diagnostics, and exit codes, so these renderers stay free
    of side effects and are unit-testable. *)

(* Compact, single-line JSON — the same serialisation [Run_report]
   uses for the prompt-run result object. *)
let to_line json = Yojson.Safe.to_string json

let error ?hint msg =
  let hint_field =
    match hint with Some h -> [ ("hint", `String h) ] | None -> []
  in
  to_line
    (`Assoc
       ([ ("error", `String msg) ] @ hint_field @ [ ("isError", `Bool true) ]))

let sessions (xs : Session_read.summary list) =
  to_line
    (`Assoc
       [
         ("kind", `String "sessions");
         ( "sessions",
           `List
             (List.map
                (fun (s : Session_read.summary) ->
                  `Assoc
                    [
                      ("id", `String s.id);
                      ("timestamp", `String s.timestamp);
                      ("count", `Int s.count);
                      ("summary", `String s.summary);
                    ])
                xs) );
       ])

let messages ~session_id (xs : Session_read.entry_summary list) =
  to_line
    (`Assoc
       [
         ("kind", `String "messages");
         ("sessionId", `String session_id);
         ( "messages",
           `List
             (List.map
                (fun (e : Session_read.entry_summary) ->
                  let parent_id =
                    match e.parent_id with Some p -> `String p | None -> `Null
                  in
                  `Assoc
                    [
                      ("id", `String e.id);
                      ("parentId", parent_id);
                      ("role", `String e.role);
                      ("preview", `String e.preview);
                      ("isFork", `Bool e.is_fork);
                    ])
                xs) );
       ])

let branches ~session_id (xs : Session_read.branch_tip list) =
  to_line
    (`Assoc
       [
         ("kind", `String "branches");
         ("sessionId", `String session_id);
         ( "branches",
           `List
             (List.map
                (fun (b : Session_read.branch_tip) ->
                  `Assoc
                    [
                      ("tip", `String b.tip);
                      ("role", `String b.role);
                      ("preview", `String b.preview);
                      ("when", `String b.timestamp);
                    ])
                xs) );
       ])

let models (pairs : (string * string) list) =
  to_line
    (`Assoc
       [
         ("kind", `String "models");
         ( "models",
           `List
             (List.map
                (fun (provider, model) ->
                  `Assoc
                    [ ("provider", `String provider); ("model", `String model) ])
                pairs) );
       ])

let default ~provider ~model =
  to_line
    (`Assoc
       [
         ("kind", `String "default");
         ("provider", `String provider);
         ("model", `String model);
       ])
