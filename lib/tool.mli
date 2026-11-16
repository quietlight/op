(** A tool that the agent can call. Each tool advertises a JSON Schema for its
    arguments and is invoked with the raw JSON arguments object. *)

type output = {
  content : string;  (** the text handed to the model *)
  details : Yojson.Safe.t option;
      (** Tool-specific metadata for the session log only — never sent to the
          model (pi's [ToolResultMessage.details]). No built-in tool currently
          returns it. *)
}
(** A successful tool call's result. *)

type t = {
  name : string;
  description : string;
  parameters : Yojson.Safe.t;
      (** JSON Schema object describing the arguments. *)
  run : Yojson.Safe.t -> (output, string) result;
      (** Run the tool with the given arguments. Returns the model-visible text
          (plus optional session-only details) on success, or a human-/model-
          readable error message. *)
}

val text : string -> (output, 'e) result
(** [text s] is [Ok] of a details-free output. The ['e] error type is free so it
    fits any {!field-run} body. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats tool [t]. *)

(* ------------------------------------------------------------------ *)
(* Argument extraction helpers for [run] implementations.             *)
(* ------------------------------------------------------------------ *)

val required_string : Yojson.Safe.t -> string -> (string, string) result
(** [required_string json field] extracts a required string [field] from [json],
    or an error message. *)

val optional_string : Yojson.Safe.t -> string -> string option
(** [optional_string json field] extracts an optional string [field] from
    [json]. *)

val optional_int : Yojson.Safe.t -> string -> int option
(** [optional_int json field] extracts an optional int [field] from [json]. *)

val optional_bool : Yojson.Safe.t -> string -> bool option
(** [optional_bool json field] extracts an optional bool [field] from [json]. *)
