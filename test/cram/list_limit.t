--list honors --limit for models (the last N, like sessions);
--list default ignores it, since it is always a single value.

  $ export HOME=$PWD
  $ umask 077  # keep the config private; op warns on world-readable models.json
  $ mkdir -p .op_global/agent
  $ cat > .op_global/agent/models.json <<'EOF'
  > {
  >   "providers": {
  >     "demo": {
  >       "baseUrl": "https://example.test/v1",
  >       "api": "openai-completions",
  >       "apiKey": "sk-x",
  >       "models": [
  >         {"id": "m-01"}, {"id": "m-02"}, {"id": "m-03"}, {"id": "m-04"},
  >         {"id": "m-05"}, {"id": "m-06"}, {"id": "m-07"}, {"id": "m-08"},
  >         {"id": "m-09"}, {"id": "m-10"}, {"id": "m-11"}, {"id": "m-12"},
  >         {"id": "m-13"}, {"id": "m-14"}, {"id": "m-15"}, {"id": "m-16"},
  >         {"id": "m-17"}, {"id": "m-18"}, {"id": "m-19"}, {"id": "m-20"},
  >         {"id": "m-21"}, {"id": "m-22"}, {"id": "m-23"}, {"id": "m-24"},
  >         {"id": "m-25"}, {"id": "m-26"}
  >       ]
  >     }
  >   }
  > }
  > EOF

Without an explicit limit, the default limit is 25 rows:

  $ op --list models
  demo/m-02
  demo/m-03
  demo/m-04
  demo/m-05
  demo/m-06
  demo/m-07
  demo/m-08
  demo/m-09
  demo/m-10
  demo/m-11
  demo/m-12
  demo/m-13
  demo/m-14
  demo/m-15
  demo/m-16
  demo/m-17
  demo/m-18
  demo/m-19
  demo/m-20
  demo/m-21
  demo/m-22
  demo/m-23
  demo/m-24
  demo/m-25
  demo/m-26
  … 1 more; use --limit N to show more

--limit keeps the last N rows (the tail), matching --list sessions:

  $ op --list models --limit 2
  demo/m-25
  demo/m-26
  … 24 more; use --limit N to show more

  $ op --list models --limit 2 --json
  {"kind":"models","models":[{"provider":"demo","model":"m-25"},{"provider":"demo","model":"m-26"}]}

--limit must be positive; zero and negative values fail during argument parsing:

  $ op --list models --limit 0
  Usage: op [--help] [OPTION]… [PROMPT]
  op: option --limit: must be a positive integer
  [124]

  $ op --list models --limit=-1
  Usage: op [--help] [OPTION]… [PROMPT]
  op: option --limit: must be a positive integer
  [124]

--list default is a single value, so a valid --limit is ignored:

  $ op --set default demo/m-01 > /dev/null
  $ op --list default --limit 1
  demo/m-01
  $ op --list default --limit 0
  Usage: op [--help] [OPTION]… [PROMPT]
  op: option --limit: must be a positive integer
  [124]
