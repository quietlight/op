(** Provider-neutral message model shared by the agent loop and adapters. *)

(** Message content block. {!constructor-Image} carries a base64 payload and
    IANA [media_type] (e.g. ["image/png"]); adapters serialize natively where
    supported or emit a placeholder. *)
type content_block =
  | Text of string
  | Image of { media_type : string; data : string  (** base64-encoded *) }

type tool_call = { id : string; name : string; arguments : Yojson.Safe.t }
(** Model-requested tool invocation; {!field-id} correlates with
    {!type-tool_result}. *)

type usage = {
  input : int;
  output : int;
  cache_read : int;
  cache_creation : int;
}
(** Normalized token counts for one response. The three prompt fields are
    disjoint: total prompt = {!field-input} + {!field-cache_read} +
    {!field-cache_creation}. OpenAI and Gemini report prompt totals that include
    cached tokens; those adapters subtract via {!usage_uncached}. *)

(** Provider reasoning ("thinking") attached to an assistant turn.

    All adapters parse reasoning for display and session logging. On replay:
    {!Anthropic_adapter} echoes signed thinking blocks verbatim and first;
    {!Google_generative_ai_adapter} echoes [signature] on the first
    [functionCall] part; OpenAI adapters do not replay reasoning.
    {!constructor-Redacted} is opaque Anthropic [redacted_thinking] ciphertext,
    replayed verbatim. *)
type thinking =
  | Reasoning of { text : string; signature : string option }
  | Redacted of string

type assistant_turn = {
  text : string;
  thinking : thinking option;
  tool_calls : tool_call list;
  usage : usage option;
}
(** One assistant response: text, optional reasoning, tool calls, and usage. *)

type tool_result = {
  id : string;  (** Matches {!type-tool_call.field-id}. *)
  name : string;
  content : string;
  is_error : bool;
  details : Yojson.Safe.t option;
      (** Tool-specific metadata logged to the session (pi's
          [ToolResultMessage.details]), never sent to the model. *)
}

type message =
  | System of string
  | User of { text : string; blocks : content_block list }
      (** User turn: [text] plus ordered attachment [blocks]. [blocks = []]
          serializes identically to a plain string user message. *)
  | Assistant of assistant_turn
  | Tool of tool_result

val usage_uncached :
  total:int -> cached:int -> output:int -> cache_creation:int -> usage
(** [usage_uncached ~total ~cached ~output ~cache_creation] subtracts [cached]
    and [cache_creation] from [total] (clamped at 0) for wire formats whose
    prompt count includes cached tokens (OpenAI, Gemini). *)

val user : ?blocks:content_block list -> string -> message
(** [user ?blocks text] builds a {!User} message ([blocks] defaults to [[]]). *)
