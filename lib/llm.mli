(** Provider-neutral message model shared by the agent loop and the provider
    adapters.

    The agent loop (agent.ml) only ever speaks in terms of these types. Each
    provider adapter is responsible for serializing a [message list] into its
    own wire format and parsing a response back into an [assistant_turn]. This
    keeps provider-specific history-validity rules out of the loop. *)

(** A neutral piece of message content. {!constructor-Text} is the ordinary
    case; a {!constructor-User} message with no attachments carries a single
    implicit text body and an empty [blocks] list. {!constructor-Image} carries
    a base64 payload and its IANA [media_type] (e.g. ["image/png"]); each
    provider adapter decides how to serialize it (native image block where the
    wire allows one, a loud text placeholder otherwise). An [Audio] constructor
    is a future addition — the variant is the seam. *)
type content_block =
  | Text of string
  | Image of { media_type : string; data : string  (** base64-encoded *) }

type tool_call = { id : string; name : string; arguments : Yojson.Safe.t }
(** A tool invocation requested by the model. {!field-id} correlates the call
    with its {!type-tool_result}; {!field-arguments} is the raw JSON argument
    object. *)

type usage = {
  input : int;
  output : int;
  cache_read : int;
  cache_creation : int;
}
(** Token counts for a single assistant response, {e normalized} across
    providers: the three prompt fields are disjoint, so the whole prompt is
    {!field-input} + {!field-cache_read} + {!field-cache_creation}, and the
    whole call is that plus {!field-output}. The two cache fields are
    prompt-caching tokens (read from / written to the cache); both are 0 for
    providers that don't report them.

    The wire formats disagree here and each adapter is responsible for the
    conversion: Anthropic already reports [input_tokens] exclusive of both cache
    counts, while OpenAI's [prompt_tokens]/[input_tokens] and Gemini's
    [promptTokenCount] are totals that {e include} the cached tokens they also
    report separately — those adapters subtract. Without that, anything summing
    the fields (the context gauge, hop's token line) double-counts a cached
    prompt. *)

(** Provider reasoning ("thinking") attached to an assistant turn.

    One rule for all adapters: every adapter *parses* a provider's reasoning
    into this type (for display and session logging), but only
    [Anthropic_adapter] *serializes* it back onto the wire — the Anthropic
    Messages API requires signed thinking blocks to be echoed back, verbatim and
    first, on a turn that also carries tool use; the OpenAI chat-completions and
    Responses APIs do not want prior reasoning resent.

    {!constructor-Reasoning} carries human-readable reasoning {!field-text} and
    an opaque provider replay token [signature] (an Anthropic thinking-block
    signature); [signature] is [None] when the provider returned none or the
    reasoning is display-only (both OpenAI adapters always set it [None]).
    {!constructor-Redacted} is Anthropic [redacted_thinking]: opaque ciphertext
    with no readable text, replayed verbatim. *)
type thinking =
  | Reasoning of { text : string; signature : string option }
  | Redacted of string

type assistant_turn = {
  text : string;
  thinking : thinking option;  (** [None] when the provider returned none. *)
  tool_calls : tool_call list;
  usage : usage option;  (** [None] when the provider reported no usage. *)
}
(** One assistant response: free text plus any tool calls it requested. *)

type tool_result = {
  id : string;  (** matches the originating {!type-tool_call.field-id} *)
  name : string;
  content : string;
  is_error : bool;
  details : Yojson.Safe.t option;
      (** Tool-specific metadata, logged to the session but never sent to the
          model — pi's [ToolResultMessage.details]. No built-in tool currently
          writes it, but foreign and historical sessions may carry it. *)
}

type message =
  | System of string
  | User of { text : string; blocks : content_block list }
      (** A user turn: free [text] plus any attached content [blocks] (images
          today). [blocks = []] is the common text-only case and every adapter
          serializes it byte-identically to a bare-string user message, so
          existing sessions, caching and tests are undisturbed. Attachment order
          is semantic and preserved. *)
  | Assistant of assistant_turn
  | Tool of tool_result

val usage_uncached :
  total:int -> cached:int -> output:int -> cache_creation:int -> usage
(** [usage_uncached ~total ~cached ~output ~cache_creation] builds a
    {!type-usage} from a wire format whose prompt count [total] {e includes} the
    cached tokens — the OpenAI and Gemini convention. [cached] and
    [cache_creation] are subtracted out of {!field-input} so the three prompt
    fields stay disjoint (see {!type-usage}); the subtraction is clamped at 0.
    Anthropic reports the fields disjoint already and builds {!type-usage}
    directly. *)

val user : ?blocks:content_block list -> string -> message
(** [user ?blocks text] builds a {!User} message; [blocks] defaults to [[]] (a
    text-only user turn). *)
