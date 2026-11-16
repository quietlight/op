(** Shared utility functions. *)

val truncate : max_bytes:int -> string -> string
(** [truncate ~max_bytes s] is [s] truncated to at most [max_bytes] bytes from
    the head, appending "..." if longer. Breaks at the last complete newline
    within the window to avoid mid-line cuts. *)

val to_valid_utf8 : string -> string
(** [to_valid_utf8 s] is [s] with every malformed UTF-8 byte sequence replaced
    by U+FFFD; already-valid input is unchanged. Used to sanitise tool output
    before it enters a request body, since some providers reject invalid UTF-8
    with HTTP 400. *)

val count_lines : string -> int
(** [count_lines s] is the number of lines in [s]. The empty string is 0 lines;
    a single trailing newline terminates the last line rather than adding an
    empty one (so ["a\n"] and ["a"] are both 1 line). *)

val substring : start:int -> needle:string -> string -> int option
(** [substring ~start ~needle s] is the 0-indexed position of the first
    occurrence of [needle] in [s] starting at [start], or [None]. *)

val escape_closing_tag : tag:string -> string -> string
(** [escape_closing_tag ~tag s] rewrites every [</tag>] in [s] to its entity
    form, so content wrapped in [<tag>...</tag>] cannot close the block early
    and have the remainder read as instructions instead of data. *)

val drop_partial_utf8_tail : string -> string
(** [drop_partial_utf8_tail s] is [s] without a trailing partial UTF-8 sequence,
    for strings cut at a byte offset. Trims truncation damage only: an invalid
    lead byte is left for {!to_valid_utf8}. *)

val home_dir : unit -> string option
(** [home_dir ()] is the user's home directory: [HOME], else (for native
    Windows) [USERPROFILE], else [HOMEDRIVE ^ HOMEPATH]. [None] when no variable
    is set. *)

val path_list_sep : string
(** [path_list_sep] separates entries in a PATH-style list: [";"] on Windows,
    [":"] elsewhere. *)

val mkdir_p : ?perm:int -> string -> unit
(** [mkdir_p ?perm dir] creates [dir] and any missing parents, like [mkdir -p].
    Existing directories are a no-op; an [EEXIST] race is ignored. Other
    {!Unix.Unix_error}s propagate. [perm] defaults to [0o755]; pass [0o700] for
    op's own state under [~/.op_global], where a world-readable directory would
    leak the session listing even though the files themselves are [0600]. *)

val read_file : string -> (string, string) result
(** [read_file path] is the whole file at [path] as raw bytes, or an error
    carrying the [Sys_error] message. Callers map the error into their own
    variant; it does not distinguish missing files from directories. *)

val next_call_id : unit -> string
(** [next_call_id ()] is a fresh process-unique tool-call id (["call_NNNNNN"]).
    Used to synthesise ids for providers that omit them. *)

val merge_adjacent : ('a * 'b list) list -> ('a * 'b list) list
(** [merge_adjacent pairs] folds consecutive [(key, list)] pairs that share a
    key into one, concatenating their lists and preserving order. *)

val human_bytes : int -> string
(** [human_bytes n] is a coarse human-readable size for [n] bytes: ["512 B"],
    ["312 KB"], ["4 MB"]. *)
