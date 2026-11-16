(** Loading and validating [--image] attachments into neutral content blocks.

    Each image is read as raw bytes, its media type sniffed from the magic bytes
    (never the extension), size-checked against {!max_bytes}, and base64-encoded
    into an {!Llm.Image} block. op does no decoding or resizing. *)

val max_bytes : int
(** Per-image raw-byte cap (5 MB); a load exceeding it is rejected. *)

val base64_encode : string -> string
(** [base64_encode s] is [s] in standard padded base64. *)

val media_type_of_bytes : string -> string option
(** [media_type_of_bytes s] sniffs the IANA media type from the leading magic
    bytes of [s], or [None] for a format outside the accepted set (png, jpeg,
    webp, gif — Anthropic's set). *)

val load : string -> (Llm.content_block, string) result
(** [load path] reads the file at [path] and returns an {!Llm.Image} block, or
    an [Error] with a clear message when the file is missing, empty, over
    {!max_bytes}, or not a supported image format. *)

val load_all : string list -> (Llm.content_block list, string) result
(** [load_all paths] loads every path in order (attachment order is semantic),
    failing on the first error. Also bounds the run as a whole — at most 5
    images, and 20 MB of encoded payload across them — since [--image] is
    repeatable and the per-image cap alone bounds nothing. *)
