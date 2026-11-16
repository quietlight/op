(** Shared helpers for the op test suite. *)

val contains : string -> string -> bool
(** [contains haystack needle] is [true] if [needle] occurs in [haystack]. *)

val write_file : string -> string -> unit
(** [write_file path content] writes [content] to the file at [path]. *)

val read_file : string -> string
(** [read_file path] is the full contents of the file at [path]. *)

val rm_rf : string -> unit
(** [rm_rf path] recursively removes the file or directory at [path]. *)

val with_temp_dir : (string -> 'a) -> 'a
(** [with_temp_dir f] creates a fresh temporary directory, passes its path to
    [f], and removes it (and its contents) afterwards. *)
