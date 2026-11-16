(** File globbing utilities. *)

type error = Invalid_pattern of string | Glob_error of string

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run : pattern:string -> ?path:string -> unit -> (string, error) result
(** [run ~pattern ?path ()] expands a glob pattern relative to an optional
    [path] and returns sorted, deduplicated file paths capped at [max_results]
    (200). *)

val tool : Tool.t
(** [tool] is the glob tool definition. *)
