The `--set default` flag persists the default model; `--model` does not.

Run op against an isolated home with one provider and no default yet. These
paths (--set / --list default) never open a session or hit the network.

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
  >       "models": [{"id": "m-1"}]
  >     }
  >   }
  > }
  > EOF

With nothing configured, --list default reports no default (exit 1):

  $ op --list default
  no default model configured; set one with op --set default PROVIDER/MODEL
  [1]

--set default saves the pair and echoes it back, same shape as --list default:

  $ op --set default demo/m-1
  demo/m-1

A fresh process reads it back from settings.json, proving it persisted:

  $ op --list default
  demo/m-1

A model the provider does not list is rejected, and the default is unchanged:

  $ op --set default demo/some-future-model
  provider 'demo' has no model 'some-future-model' (run op --list models)
  [1]
  $ op --list default
  demo/m-1

--json echoes the new default as one object on stdout:

  $ op --set default demo/m-1 --json
  {"kind":"default","provider":"demo","model":"m-1"}

A bare model (no provider) is rejected before anything is written:

  $ op --set default justmodel
  --set default needs an explicit provider: PROVIDER/MODEL
  [1]

An unknown provider is rejected too:

  $ op --set default bogus/m-1
  unknown provider 'bogus'
  [1]

With no argument at all:

  $ op --set default
  --set default requires a PROVIDER/MODEL argument
  [1]

The default is unchanged after the rejected attempts:

  $ op --list default
  demo/m-1
