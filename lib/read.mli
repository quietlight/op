(** Read text files with line numbers. *)

type error =
  | Not_found of string
  | Is_directory of string
  | Read_error of string

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run :
  path:string -> ?offset:int -> ?limit:int -> unit -> (string, error) result
(** [run ~path ?offset ?limit ()] is the contents of the file at [path] with
    1-indexed line numbers, starting at [offset] (default 1) and reading at most
    [limit] lines (default 2000). Individual lines are truncated to a fixed
    width and the whole result to a byte budget; if either cap or the line limit
    stops the read short, a trailing "... (N more lines)" note reports how many
    lines remain. *)

val tool : Tool.t
(** [tool] is the read tool definition. *)
