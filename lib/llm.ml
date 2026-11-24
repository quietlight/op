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

let usage_uncached ~total ~cached ~output ~cache_creation =
  {
    input = max 0 (total - cached - cache_creation);
    output;
    cache_read = cached;
    cache_creation;
  }
