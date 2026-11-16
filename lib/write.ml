let ( let* ) = Result.bind

type error = Write_error of string | Mkdir_error of string

let pp_error = function Write_error msg | Mkdir_error msg -> msg

let run ~path ~content =
  match Util.mkdir_p (Filename.dirname path) with
  | exception Unix.Unix_error (e, _, _) ->
      Error (Mkdir_error (Unix.error_message e))
  | () -> (
      try
        Out_channel.with_open_bin path (fun oc ->
            Out_channel.output_string oc content);
        Ok ()
      with Sys_error msg -> Error (Write_error msg))

let tool : Tool.t =
  {
    name = "write";
    description =
      "Write a file to the filesystem, overwriting it if it exists and \
       creating parent directories as needed.";
    parameters =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Path of the file to write");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Full contents to write");
                    ] );
              ] );
          ("required", `List [ `String "path"; `String "content" ]);
        ];
    run =
      (fun args ->
        let* path = Tool.required_string args "path" in
        let* content = Tool.required_string args "content" in
        match run ~path ~content with
        | Ok () ->
            Fmt.kstr Tool.text "Wrote %d bytes to %s" (String.length content)
              path
        | Error e -> Error (pp_error e));
  }
