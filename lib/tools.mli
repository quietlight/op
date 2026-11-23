(** Built-in tool registry. *)

val known : string list
(** [known] is every built-in tool name, in canonical order:
    [read; write; edit; nu]. Used to validate [--tools], to order a selection,
    and as the default selection when neither [--tools] nor the run's profile
    picks one. *)

val validate : string list -> (string list, string) result
(** [validate names] is [names] when every element is a member of {!known}, and
    otherwise an error naming the first unknown tool and the known set. The
    shared check behind [--tools] and a profile's [tools] field; an empty list
    is valid — it is how a tool-less (chat) run is requested. *)

val select : nu:Tool.t option -> string list -> Tool.t list
(** [select ~nu requested] is the tools named in [requested], emitted in
    {!known} order (duplicates collapsed, unknown names ignored). The
    caller-built, Eio-backed [nu] tool is included only when both requested and
    supplied here; a [None] builder for a requested [nu] drops it. *)
