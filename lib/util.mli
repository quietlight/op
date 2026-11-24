(** Shared utility functions. *)

val truncate : max_bytes:int -> string -> string
(** [truncate ~max_bytes s] keeps at most [max_bytes] bytes from the head,
    breaking at the last newline or UTF-8 boundary and appending ["..."]. *)

val to_valid_utf8 : string -> string
(** [to_valid_utf8 s] replaces malformed UTF-8 sequences with U+FFFD. *)

val count_lines : string -> int
(** [count_lines s] counts lines ([""] -> 0; a trailing ['\n'] does not add a
    line). *)

val substring : start:int -> needle:string -> string -> int option
(** [substring ~start ~needle s] returns the first 0-indexed position of
    [needle] in [s] at or after [start]. *)

val escape_closing_tag : tag:string -> string -> string
(** [escape_closing_tag ~tag s] rewrites [</tag>] to [&lt;/tag&gt;] so wrapped
    data cannot close its own block. *)

val drop_partial_utf8_tail : string -> string
(** [drop_partial_utf8_tail s] trims a trailing partial UTF-8 sequence after a
    byte cut (leaves invalid lead bytes to {!to_valid_utf8}). *)

val home_dir : unit -> string option
(** [home_dir ()] returns [HOME], [USERPROFILE], or [HOMEDRIVE ^ HOMEPATH]. *)

val path_list_sep : string
(** [";"] on Windows, [":"] elsewhere. *)

val mkdir_p : ?perm:int -> string -> unit
(** [mkdir_p ?perm dir] creates [dir] and missing parents (default [0o755]; use
    [0o700] for [~/.op_global]). Ignores existing directories and [EEXIST]. *)

val read_file : string -> (string, string) result
(** [read_file path] reads [path] as raw bytes, returning [Sys_error] messages
    as [Error]. *)

val read_file_capped : max:int -> string -> (string, string) result
(** [read_file_capped ~max path] checks [Unix.stat] before reading; rejects
    non-regular files or files over [max] bytes. *)

val next_call_id : unit -> string
(** [next_call_id ()] returns a process-unique synthetic tool-call id
    (["call_NNNNNN"]). *)

val merge_adjacent : ('a * 'b list) list -> ('a * 'b list) list
(** [merge_adjacent pairs] merges consecutive [(key, list)] pairs with equal
    keys. *)

val human_bytes : int -> string
(** [human_bytes n] formats [n] as ["512 B"], ["312 KB"], or ["4 MB"]. *)
