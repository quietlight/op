(* Provider-neutral message model shared by the agent loop and the provider
   adapters. See llm.mli. No wire-format details leak in here: e.g. Anthropic
   tool_use/tool_result shapes live in anthropic_adapter.ml. *)

type content_block =
  | Text of string
  | Image of { media_type : string; data : string (* base64 *) }

type tool_call = { id : string; name : string; arguments : Yojson.Safe.t }

type usage = {
  input : int;
  output : int;
  cache_read : int;
  cache_creation : int;
}

type thinking =
  | Reasoning of { text : string; signature : string option }
  | Redacted of string

type assistant_turn = {
  text : string;
  thinking : thinking option;
  tool_calls : tool_call list;
  usage : usage option;
}

type tool_result = {
  id : string;
  name : string;
  content : string;
  is_error : bool;
  details : Yojson.Safe.t option;
}

type message =
  | System of string
  | User of { text : string; blocks : content_block list }
  | Assistant of assistant_turn
  | Tool of tool_result

let user ?(blocks = []) text = User { text; blocks }

(* OpenAI and Gemini report a prompt total that already contains the cached
   tokens they break out separately; usage keeps the three prompt fields
   disjoint, so those adapters subtract here. Clamped at 0: a provider that
   ever reported a cache count larger than its own total must not produce a
   negative prompt. *)
let usage_uncached ~total ~cached ~output ~cache_creation =
  {
    input = max 0 (total - cached - cache_creation);
    output;
    cache_read = cached;
    cache_creation;
  }
