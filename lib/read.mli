(** Read text files with line numbers. *)

type error =
  | Not_found of string
  | Is_directory of string
  | Read_error of string

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run :
  path:string -> ?offset:int -> ?limit:int -> unit -> (string, error) result
(** [run ~path ?offset ?limit ()] streams [path] (regular files only) with
    1-indexed line numbers from [offset] (default 1) for up to [limit] lines
    (default 2000), capping line width and total bytes. Appends a continuation
    offset note if truncated. *)

val tool : Tool.t
(** [tool] is the read tool definition. *)
