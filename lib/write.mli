(** Write files and create directories. *)

type error = Write_error of string | Mkdir_error of string

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run : path:string -> content:string -> (unit, error) result
(** [run ~path ~content] writes [content] to [path], overwriting any existing
    file and creating parent directories as needed. *)

val tool : Tool.t
(** [tool] is the write tool definition. *)
