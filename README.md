# op

**A one-shot agentic loop for the command line.**

op reads a prompt, runs a model through a tool loop until it is done, prints
the transcript, and exits. That is the whole interface. There is no chat mode,
no TUI, no daemon: each call is a plain Unix command that composes with pipes,
scripts, cron, and other copies of itself.

```
$ op --tools read,nu "what does lib/gate.mli guard against?"
```

The design lineage is [pi](https://github.com/badlogic/pi-mono): op writes the
same session format, so the two can read each other's history.

## Why you might want it

- **One shot, by design.** A run is a function of its inputs. Resume or branch
  a session explicitly when you want to continue; otherwise nothing lingers.
- **Nushell instead of bash.** The only execution tool is a Nushell script,
  vetted by a static AST gate before it runs. Structured data in, structured
  data out, and no shell quoting accidents.
- **Capability profiles.** A profile names exactly which external commands,
  Nushell modules, plugins and environment variables a run may reach. The
  default is a builtins-only Nushell with nothing else on the path.
- **No compaction.** When the context window fills, the run stops cleanly and
  you hand the work off to a fresh session with a generated brief, instead of
  a summary stacked on a summary.
- **Provider-agnostic.** Anthropic, OpenAI-compatible, OpenAI Responses and
  Google Generative AI APIs, configured in one JSON file. No built-in
  providers, no hidden accounts.
- **Machine-readable when asked.** `--json` gives a result envelope,
  `--stream` gives the session as newline-delimited JSON. Scripts and
  subagents never have to scrape a transcript.

## Requirements

- Linux or another POSIX host. Windows is not supported; use WSL.
- [Nushell](https://www.nushell.sh/) 0.117 on your PATH, if you want the
  `nu` tool. Without it op still runs with the file tools only.
- OCaml 5.x with opam and dune 3.23+, to build from source.
- An API key for at least one provider.

## Install

```
git clone https://github.com/quietlight/op
cd op
opam install .
```

`opam install .` builds op with its dependencies and puts the `op` executable
in your opam switch's `bin`, which is already on your PATH when the switch is
active.

## Configure

op reads two files from `~/.op_global/agent/`. Create the directory and add
at least one provider to `models.json`:

```json
{
  "providers": {
    "anthropic": {
      "baseUrl": "https://api.anthropic.com",
      "api": "anthropic-messages",
      "apiKeyEnv": "ANTHROPIC_API_KEY",
      "models": [
        { "id": "claude-sonnet-5", "contextWindow": 200000 }
      ]
    },
    "openai": {
      "baseUrl": "https://api.openai.com/v1",
      "api": "openai-responses",
      "apiKey": "sk-...",
      "models": [
        { "id": "gpt-5.6", "contextWindow": 400000, "maxTokens": 32768 }
      ]
    }
  }
}
```

- `api` is one of `anthropic-messages`, `openai-completions`,
  `openai-responses` or `google-generative-ai`. It defaults to
  `openai-completions`, which covers most third-party hosts.
- Give the key inline with `apiKey`, or name an environment variable with
  `apiKeyEnv` to keep it off disk. Setting both is an error.
- `contextWindow` turns on context reporting and the context cap for that
  model. `maxTokens` caps the response length. Both are optional.

Then pick a default model:

```
op --set default anthropic/claude-sonnet-5
```

That writes `settings.json`. Everything else in that file is optional:

```json
{
  "defaultProvider": "anthropic",
  "defaultModel": "claude-sonnet-5",
  "contextLimit": 0.85,
  "handoffModel": "anthropic/claude-opus-5",
  "nuDefaultProfile": "chat",
  "nuProfiles": {
    "chat": { "tools": [] },
    "dev": {
      "externals": ["make", "git", "op"],
      "moduleDirs": ["~/code/nu_modules"],
      "plugins": ["polars"],
      "pathMode": "host",
      "envAllow": []
    }
  }
}
```

| key                | what it does                                                        |
|--------------------|---------------------------------------------------------------------|
| `contextLimit`     | Fraction of the context window at which a run stops (default 0.85). |
| `handoffModel`     | Model that writes `--handoff` briefs. Use a strong one with a large window. |
| `nuProfiles`       | Named capability profiles. See [Profiles](#profiles).               |
| `nuDefaultProfile` | Profile used when `--profile` is not given.                         |

## Use

```
op "prompt"                    # human transcript on stdout
echo "prompt" | op             # prompt from stdin
op --prompt task.md            # prompt from a file
op --context CLAUDE.md "…"     # inject a file ahead of the prompt (repeatable)
op --image shot.png "…"        # attach an image (png, jpeg, webp, gif)
op --model openai/gpt-5.6 "…"  # override the model for one run
op --profile dev "…"           # pick a capability profile
op --tools read,nu "…"         # pick a tool subset, or `none`
op --json "…"                  # result envelope only
op --stream "…"                # session ndjson, then the envelope
op --max-steps 3 "…"           # cap the number of tool-using turns
op --render --sessionId ID     # replay a session as the transcript
op --stream "…" | op --render  # the same transcript, rendered from the stream
```

The transcript shows each assistant turn, each tool call with its arguments,
a preview of each result, and a summary line with token counts, the context
gauge and a ready-to-paste resume command. It is coloured on a terminal and
plain down a pipe.

`op --help` is the full reference for flags, exit codes and examples.

### Tools

The tool set is `read`, `write`, `edit` and `nu`. With no profile and no
`--tools`, a run has no tools at all: a plain chat. Ask for tools with
`--tools` or `--profile`. A profile can set a default subset (a profile
without a `tools` list gets all four), and `--tools` overrides it for one run. The `nu`
tool is the only way to execute anything. There is no bash tool.

### Sessions

Every run appends to a session under `~/.op_global/sessions/`. A session is a
tree: resume from its leaf, or branch from any earlier entry.

```
op --list sessions
op --list messages --sessionId ID
op --sessionId ID "now add tests"
op --sessionId ID --parentId ENTRY "try another approach"
```

Once a session has branched it has several leaves, so `--sessionId` alone is
ambiguous. `op --list branches --sessionId ID` names one tip per branch, and
`--parentId` with that tip continues it.

The file format is documented in
[docs/pi-session-format.md](docs/pi-session-format.md).

### Context and handoff

op does not compact. When a model has a `contextWindow`, every result carries
a `context` object (tokens, window, percent), and the run stops between turns
once it passes `contextLimit`. The check uses the usage the provider last
reported, so it is a stopping threshold rather than a cap on the next request.
The history is complete at that point, so nothing is lost.

To carry on, generate a handoff brief and feed it to a fresh session:

```
op --handoff --sessionId ID "execute phase one of the plan" | op --fromSession ID
```

The brief covers goal, constraints, progress, decisions, next steps, and the
files read and modified. The file lists come from session state, not the model,
so nothing is dropped or invented. The new session records the old one as its
parent.

## The `nu` tool

Each `nu` call is a Nushell script. Before it runs, a static gate parses it
and rejects anything the profile does not allow:

- external commands outside the profile's `externals`
- computed command heads such as `^$cmd`, and path-based dispatch
- the network, exec and dispatch builtins (`http`, `run-external`, `exec`,
  `source`, `plugin` and friends), and anything that loads a file the gate has
  not read (`use`/`export use`/`module` of a path)
- setting `PATH` or op's `OP_*` variables (they may only be read)

Scripts that name sensitive paths (ssh keys, cloud credentials, `.env`, shell
history, `/proc`) or write dot-files are denied too. That check is a text match
on the script, so it stops an incidental read, not a determined one. The file
tools refuse the same paths, and any path with a dot-prefixed segment.

The script then runs with a PATH containing only the profile's externals, and
an environment that is an allowlist, not an inheritance. Only `HOME`, `USER`,
`LOGNAME`, `LANG`, `LC_ALL`, `TERM`, `TMPDIR`, `TZ`, op's own `OP_*` variables
and whatever the profile's `envAllow` names get through. A stray
`ANTHROPIC_API_KEY` or `SSH_AUTH_SOCK` is not in the script's environment.
(op's own environment is still readable under `/proc`, which is why that path
is refused — by the same best-effort text match.)

This is a guardrail for a cooperative model, not a sandbox against a hostile
one. For untrusted work, wrap op in a real sandbox as well.

### Profiles

A profile in `nuProfiles` is the full capability knob for a run:

| field        | meaning                                                              |
|--------------|----------------------------------------------------------------------|
| `tools`      | Default tool set. `[]` makes a tool-less chat profile.               |
| `externals`  | External commands the script may call. Symlinked into a per-run toolbox. |
| `moduleDirs` | Directories whose `*.nu` files may be imported with `use`. Each module is re-read and gated on every call. |
| `plugins`    | Nushell plugins the script may load, e.g. `polars`.                   |
| `pathMode`   | `"host"` lets allowlisted build drivers (`make`, `go`, `opam`) find their own children (`gcc`, `git`). Default is toolbox-only. |
| `envAllow`   | Extra environment variable names passed through to the script.       |

With no profile at all, a run has no tools; `--tools nu` then gives the script
a builtins-only Nushell. That is still useful: Nushell has a builtin for most
things you would shell out for.

Network access from a script is done by calling op itself. Add `op` to
`externals` and a script can run `op --json "…" | from json` as a subagent.

### What op does not protect

op limits what the agent is handed; it does not contain what it is handed.
Choose tools and profiles knowing these grants:

- **Builtins-only nu is not read-only.** `rm`, `mv`, `cp` and `save -f` work
  on any path you can write, on every mounted drive. For a read-only run use
  `--tools read`.
- **Reading is sending.** Whatever a tool reads goes to your model provider
  on the next turn and into the session file. Pick the provider for the data.
- **An external is a trust grant, not a name.** `make` runs any Makefile,
  `opam exec` runs anything, `git -c alias.x='!…'` runs anything, and `git`
  reaches the network. `op` in `externals` can start a nested run under any
  profile and read every saved session.
- **Path checks are textual.** Symlinks, paths assembled at runtime and
  check-then-use races get past them. They stop accidents.
- **Text you read can steer the model.** A file on any drive may carry
  instructions. For untrusted inputs, run op inside an OS sandbox.

## Skills

A skill is a markdown file under `$cwd/skills/`, with an optional
`description:` line in its frontmatter. Skills are injected like any other
context file:

```
op --context skills/practical-nu.md "summarise the parquet files in data/"
```

This repo ships one,
[skills/practical-nu.md](skills/practical-nu.md): a playbook for getting
things done inside the gated Nushell environment. It is worth injecting on
any run that uses the `nu` tool.

## hop

hop is an interactive front end written in Nushell, kept outside this
repository. It threads each run's session id into the next call, renders turns
live by piping `--stream` into `op --render`, offers `@file` context
completion, and drives the handoff flow from an editor. op stays a one-shot
primitive; hop is where the back-and-forth lives, and everything it does goes
through the flags documented here.

## Developing

```
opam install . --deps-only --with-test
dune build @check    # fast type-check
dune build           # build; the executable is _build/default/bin/main.exe
dune fmt             # format
dune runtest         # unit and cram tests
```

## License

MIT. See [LICENSE.md](LICENSE.md).
