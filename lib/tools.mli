(** Built-in tool registry. *)

val known : string list
(** Built-in tool names in canonical order: [["read"; "write"; "edit"; "nu"]].
*)

val validate : string list -> (string list, string) result
(** [validate names] checks every name is in {!known} ([[]] is valid for
    tool-less chat runs). *)

val select : nu:Tool.t option -> string list -> Tool.t list
(** [select ~nu requested] returns requested tools in {!known} order, collapsing
    duplicates and dropping ["nu"] when [nu] is [None]. *)
