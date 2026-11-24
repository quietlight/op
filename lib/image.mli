(** Loading and validating [--image] attachments into {!Llm.Image} blocks.

    Sniffs media type from magic bytes, enforces size caps, and base64-encodes
    without decoding or resizing. *)

val max_bytes : int
(** Per-image raw-byte cap (5 MB). *)

val base64_encode : string -> string
(** [base64_encode s] encodes [s] as standard padded base64. *)

val media_type_of_bytes : string -> string option
(** [media_type_of_bytes s] sniffs PNG, JPEG, WebP, or GIF from magic bytes. *)

val load : string -> (Llm.content_block, string) result
(** [load path] reads, validates, and base64-encodes one image file. *)

val load_all : string list -> (Llm.content_block list, string) result
(** [load_all paths] loads images in order, capping a run at 5 images and 20 MB
    of total encoded payload. *)
