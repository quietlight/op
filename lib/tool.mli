(** Agent tool interface and argument extraction helpers. *)

type output = {
  content : string;  (** Text returned to the model. *)
  details : Yojson.Safe.t option;
      (** Session-only metadata (pi's [ToolResultMessage.details]), never sent
          to the model. *)
}
(** Successful tool result. *)

type t = {
  name : string;
  description : string;
  parameters : Yojson.Safe.t;  (** JSON Schema for tool arguments. *)
  run : Yojson.Safe.t -> (output, string) result;
}

val text : string -> (output, 'e) result
(** [text s] wraps [s] in [Ok { content = s; details = None }]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats tool [t]. *)

val required_string : Yojson.Safe.t -> string -> (string, string) result
(** [required_string json field] extracts a required string [field] from [json].
*)

val optional_string : Yojson.Safe.t -> string -> string option
(** [optional_string json field] extracts an optional string [field] from
    [json]. *)

val optional_int : Yojson.Safe.t -> string -> int option
(** [optional_int json field] extracts an optional int [field] from [json]. *)

val optional_bool : Yojson.Safe.t -> string -> bool option
(** [optional_bool json field] extracts an optional bool [field] from [json]. *)
