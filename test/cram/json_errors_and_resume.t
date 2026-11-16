JSON error envelopes stay compact and list commands reject stray prompt
arguments.

  $ export HOME=$PWD
  $ umask 077  # keep the config private; op warns on world-readable models.json

Unreadable prompt files report the path once.

  $ op --prompt /no/such/file --json
  {"error":"/no/such/file: No such file or directory","isError":true}
  [1]

Missing models.json keeps the JSON error short and puts the setup text in a
hint field.

  $ op --json "hi" | python3 -c 'import json,sys; o=json.load(sys.stdin); print(o["error"]); print("hint" in o); print(o["isError"])'
  no models.json found in $TESTCASE_ROOT/.op_global/agent
  True
  True

--json and --stream both own stdout, so the combination is a usage error
rather than a silent precedence rule.

  $ op --json --stream "hi"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: --json and --stream are mutually exclusive
  [124]

A hard failure under --stream is the error envelope, tagged so it is
distinguishable from the session-entry lines it shares stdout with.

  $ op --stream --prompt /no/such/file
  {"type":"result","error":"/no/such/file: No such file or directory","isError":true}
  [1]

The human transcript is the default, so a bare run says it in prose on stderr
— stdout carries the transcript and never a JSON object.

  $ op --prompt /no/such/file
  error: /no/such/file: No such file or directory
  [1]


--max-steps must be positive.

  $ op --max-steps=0 "hi"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: option --max-steps: must be a positive integer
  [124]

  $ op --max-steps=-1 "hi"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: option --max-steps: must be a positive integer
  [124]

  $ mkdir -p .op_global/agent
  $ cat > .op_global/agent/models.json <<'EOF'
  > {
  >   "providers": {
  >     "demo": {
  >       "baseUrl": "https://example.test/v1",
  >       "api": "openai-completions",
  >       "apiKey": "sk-x",
  >       "models": [{"id": "m-1"}]
  >     }
  >   }
  > }
  > EOF
  $ op --set default demo/m-1 > /dev/null

List commands reject stray prompt arguments.

  $ op --list default bogus
  --list does not accept a PROMPT argument
  [1]

  $ op --list default bogus --json
  {"error":"--list does not accept a PROMPT argument","isError":true}
  [1]

--handoff needs a session to summarise, and writes no session entries of its
own, so it has no stream to share with --stream.

  $ op --handoff "next goal"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: --handoff requires --sessionId
  [124]

  $ op --handoff --stream --sessionId abc "next goal"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: --handoff and --stream are mutually exclusive
  [124]

--fromSession starts a fresh session that records its origin; --sessionId
resumes one. Asking for both is a usage error rather than a precedence rule.

  $ op --fromSession abc --sessionId def "hi"
  Usage: op [--help] [OPTION]… [PROMPT]
  op: --fromSession and --sessionId are mutually exclusive
  [124]

An unknown --fromSession id fails before the run starts.

  $ op --json --fromSession no-such-session "hi" | python3 -c 'import json,sys; o=json.load(sys.stdin); print(o["error"].split(" not found")[0]); print(o["isError"])'
  --fromSession: session no-such-session
  True
