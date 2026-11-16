(** File-editing primitives. *)

type error =
  | Not_found of string
  | Read_error of string
  | Write_error of string
  | Substring_not_found
  | Not_unique of int
  | No_change

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run :
  path:string ->
  old_string:string ->
  new_string:string ->
  ?replace_all:bool ->
  unit ->
  (int, error) result
(** [run ~path ~old_string ~new_string ?replace_all ()] replaces [old_string]
    with [new_string] in the file at [path]. [old_string] must occur exactly
    once unless [replace_all] is set. Returns the number of replacements made.
*)

val tool : Tool.t
(** [tool] is the edit tool definition. *)
