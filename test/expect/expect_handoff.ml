open Op

(* The handoff document's deterministic half. The model writes the prose; op
   writes the file blocks, so those are what must be pinned — a hallucinated
   path is exactly the loss a handoff exists to prevent. *)

let call ~name ~path : Llm.tool_call =
  {
    id = name ^ ":" ^ path;
    name;
    arguments = `Assoc [ ("path", `String path) ];
  }

let assistant tool_calls =
  Llm.Assistant { text = ""; thinking = None; tool_calls; usage = None }

(* The result answering [tc]. A path counts only once its call came back
   without an error, so every fixture must say how the call actually went. *)
let result ?(is_error = false) (tc : Llm.tool_call) =
  Llm.Tool
    { id = tc.id; name = tc.name; content = "ok"; is_error; details = None }

(* An assistant turn plus a successful result for each of its calls. *)
let turn calls = assistant calls :: List.map result calls

let show_files (f : Handoff.files) =
  Fmt.pr "read=[%s] modified=[%s]@." (String.concat ";" f.read)
    (String.concat ";" f.modified)

let%expect_test "files_of_history: read vs modified, deduped, in order" =
  let history =
    [
      [ Llm.user "go" ];
      turn [ call ~name:"read" ~path:"lib/a.ml" ];
      turn
        [
          call ~name:"read" ~path:"lib/b.ml"; call ~name:"edit" ~path:"lib/a.ml";
        ];
      (* a second read of the same file adds nothing *)
      turn
        [
          call ~name:"read" ~path:"lib/a.ml";
          call ~name:"write" ~path:"lib/c.ml";
        ];
    ]
    |> List.concat
  in
  show_files (Handoff.files_of_history history);
  [%expect {| read=[lib/a.ml;lib/b.ml] modified=[lib/a.ml;lib/c.ml] |}]

(* glob's "path" is a search root and its "pattern" names no file, so counting
   it would send the next thread rummaging in directories nobody opened. *)
let%expect_test "files_of_history: glob and pathless calls are ignored" =
  let history =
    turn
      [
        call ~name:"glob" ~path:"lib";
        {
          id = "x";
          name = "nu";
          arguments = `Assoc [ ("script", `String "ls") ];
        };
      ]
  in
  show_files (Handoff.files_of_history history);
  [%expect {| read=[] modified=[] |}]

(* A denied or failed call leaves the file untouched. Listing it would be the
   invention the handoff document promises not to make — and a next thread told
   an edit landed when it did not is worse than one told nothing. *)
let%expect_test "files_of_history: failed and unanswered calls do not count" =
  let denied = call ~name:"edit" ~path:"lib/denied.ml" in
  let missing = call ~name:"read" ~path:"lib/missing.ml" in
  let ok = call ~name:"write" ~path:"lib/done.ml" in
  let history =
    [
      assistant [ denied; missing; ok ];
      result ~is_error:true denied;
      result ~is_error:true missing;
      result ok;
      (* a turn cut off before its results were recorded *)
      assistant [ call ~name:"write" ~path:"lib/cut-off.ml" ];
    ]
  in
  show_files (Handoff.files_of_history history);
  [%expect {| read=[] modified=[lib/done.ml] |}]

(* A session that touched nothing gets no file blocks — the document stays the
   model's prose alone. *)
let%expect_test "assemble: empty sections are omitted" =
  print_string
    (Handoff.assemble ~summary:"## Goal\nJust talk.\n"
       ~files:{ read = []; modified = [] });
  [%expect {|
    ## Goal
    Just talk.
    |}]

let%expect_test "user_prompt: an empty goal asks the model to infer one" =
  print_endline (Handoff.user_prompt ~goal:"");
  print_endline "---";
  print_endline (Handoff.user_prompt ~goal:"execute phase one");
  [%expect
    {|
    Write the handoff prompt for a fresh thread continuing this work.

    The user gave no explicit next goal: infer it from where the work left off, and say under Goal that you inferred it.
    ---
    Write the handoff prompt for a fresh thread continuing this work.

    The user's goal for the new thread is: execute phase one
    |}]
