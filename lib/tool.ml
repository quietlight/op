type output = { content : string; details : Yojson.Safe.t option }

type t = {
  name : string;
  description : string;
  parameters : Yojson.Safe.t;
  run : Yojson.Safe.t -> (output, string) result;
}

let text content = Ok { content; details = None }
let pp fmt t = Fmt.pf fmt "@[<v>tool %s: %s@]" t.name t.description

let err_missing_param field =
  Fmt.kstr (fun s -> Error s) "missing required parameter %S" field

let err_not_string field =
  Fmt.kstr (fun s -> Error s) "parameter %S must be a string" field

let required_string json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> Ok s
  | `Null -> err_missing_param field
  | _ -> err_not_string field

let optional_string json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | _ -> None

let optional_int json field =
  match Yojson.Safe.Util.member field json with `Int n -> Some n | _ -> None

let optional_bool json field =
  match Yojson.Safe.Util.member field json with `Bool b -> Some b | _ -> None
