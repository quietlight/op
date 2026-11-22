---
description: How to actually get things done in op's gated Nushell env — builtins over Unix tools, capturing output, common gotchas
---
**PLEASE EDIT THIS IF YOU FIND HOLES: KEEP IT TERSE, USEFUL**

You are operating in op's `nu` tool: a Nushell script vetted by a static gate
before it runs, under a toolbox PATH that contains **only** a few allowed
external commands. Most bash reflexes fail here. Work *with* Nushell, not against
it.

## The one rule that prevents most failures

**Externals are limited to the toolbox — the allowed names are listed in the
nu tool's own description; they come from the run's profile.
Everything else must be a Nushell builtin.** Reaching for a Unix tool — or
forcing an external with `^cmd` — gets gate-blocked. Nushell has a builtin for
almost everything you'd shell out for:

| bash reflex        | do this instead (builtin)                                  |
|--------------------|------------------------------------------------------------|
| `echo x`           | `print x`  (never `^echo`)                                 |
| `cat f`            | `open --raw f`                                             |
| `head -n5 f`       | `open --raw f \| lines \| first 5`                         |
| `tail -n5 f`       | `open --raw f \| lines \| last 5`                          |
| `grep pat f`       | `open --raw f \| lines \| enumerate \| where item =~ pat`  |
| `wc -l f`          | `open --raw f \| lines \| length`                          |
| `ls`, `find`       | builtin `ls **/*` (skips dotfiles; `glob` does not)        |
| `sed`/`awk`        | `str replace`, `each { ... }`, `parse "..."`               |
| `curl`/`http`      | nested `op --json "prompt" \| from json` (op is toolbox)   |

`op` is itself a toolbox external: a one-shot agentic subagent. `--json` makes
it emit a single result object on stdout (instead of the default entry
stream) — parse with
`from json` and read `.answer`. Run `op --help` for the full surface (sessions,
`--max-steps`, `--tools`, `--model`, `--profile`).

## Parallel subagents: `par-each` over nested `op`

`par-each` passes the gate and gives real parallelism (N sleeps of 1s finish in
~1s). Fan a batch of prompts out to op subagents like this:

    $prompts | par-each --keep-order { |q|
      $q | ^op --json --model anthropic/claude-haiku-4-5 --max-steps 8
         | from json | get answer
    }

Two rules learned the hard way:

- **Feed the prompt on stdin, not as an argument.** Inside a `par-each` closure
  the item is also the closure's pipeline input, and a leading external inherits
  it as stdin — `^op --json $q` then dies with *"ambiguous input: a PROMPT
  argument and stdin were both provided"*. `$q | ^op --json` resolves it.
- `par-each` returns results in **completion order**; add `--keep-order` to
  match input order.

Wall time ≈ the slowest call, not the sum (4 haiku calls ≈ 1.8s). Recursion is
bounded by `OP_DEPTH` (op→nu→op→nu→op is the floor).

For fire-then-work-then-join (start a subagent, keep working, collect later),
`job spawn` + `job recv` also passes the gate:

    let id = (job spawn { let r = ("prompt" | ^op --json | from json | get answer); $r | job send 0 })
    # ... foreground work ...
    let sub = (job recv --timeout 60sec)

**Jobs die when the script exits** — nu kills background jobs at the end of the
run, so a spawned job's work is silently lost unless you `job recv` (or
otherwise join) before the script ends. Job closures are gate-vetted like all
other code: toolbox externals only, no `http`.

## Waiting: timers, deadlines, polling

No timer tool is needed; plain nu covers it, and you control the deadline
yourself:

- `sleep 30sec` blocks for a set time.
- `job recv --timeout 5min` waits for a spawned job and **errors** on timeout
  (*"No message was received in the requested time interval"*). Wrap it in
  `try { ... } catch { ... }` if a timeout should not fail the script.
- Polling (wait for a file or condition):
  `loop { if ($p | path exists) { break }; sleep 5sec }`. Bound it with a
  counter or a `(date now)` deadline.
- **Raise the nu tool's `timeout` argument** (seconds, default 120) for any
  wait longer than about 2 minutes, or the whole script is killed, and your
  jobs die with it.

The wait has to happen *inside one nu call*. Nothing (jobs, plugins, timers)
outlives the script. If you need a subagent's result, join it before you end
your turn. op has no "wake me later".

## Web: `web search` / `web extract` (profile `web`)

Where a profile lists the `web` plugin, start the script with `plugin use web`.

    web search 'How is kauri dieback spread?' 'kauri dieback spread' 'kauri dieback hygiene' | to yaml
    web search 'kauri hygiene rules' -n 2 | web extract --objective 'boot cleaning steps' | to yaml

- **Always end with `| to yaml`.** nu's default table shows `excerpts` as
  `[list 1 item]`, so you see no text at all.
- The first argument is an objective (one sentence: what you want and why).
  The rest are 1-3 short keyword queries. Don't repeat the objective as a
  query.
- The excerpts usually answer the question. Use `web extract` (with
  `--objective`, or `--full` for whole pages) only when they don't.

## Capturing output and exit codes

There is **no `2>&1`** — bash redirection doesn't parse and the gate blocks it.

- `some-cmd | complete` → a record `{ stdout, stderr, exit_code }`. This is the
  workhorse: `let r = (make | complete); $r.exit_code`.
- Wrap a whole step to capture even when it errors: `do { opam exec -- dune build } | complete`.
- Stream merge into a pipe: `cmd o+e>| lines`. Stderr only: `cmd e>| lines`.
- To a file: `cmd out> f`, `cmd err> f`, `cmd out+err> f`.

## `open` and structured data

`open f` **auto-parses by extension** (`.json`→value, `.toml`, `.csv`, …). When
you want raw text, use `open --raw f`. Then `| lines` to split, `| from json` to
parse explicitly.

`enumerate` turns a list into rows shaped `{ index, item }` — filter with
`where item =~ "regex"` or a closure `where {|r| $r.item =~ "..."}`. There is no
`.content` field; that mistake yields `IncompatiblePathAccess`.

**`enumerate` inside nested closures is fragile.** `$r.index` / `$r.item` can
resolve against the wrong scope and throw `CantFindColumn` / `DidYouMean` when
you nest closures (e.g. an `each` inside a `where`). Don't fight it — flatten the
pipeline (do the `enumerate`/filter at one level, bind the row to an explicit
closure param `{|r| ...}`, or capture the needed field into a `let` before
nesting). This is a real time-sink if you try to diagnose it in place.

A grep-across-files reflex becomes a Nushell pipeline — sweep with `ls`, not
`glob`: `ls **/*` skips dotfiles, `glob "**/*"` happily returns `.env`:

    ls **/*.ml | get name | each {|f| open --raw $f | lines | enumerate
      | where item =~ "pat" | each {|r| {file: $f, line: ($r.index + 1), text: $r.item}} }
      | flatten

## Running the build gate

`make` works directly:

    make                                  # runs fmt/build/runtest + merlint
    make check | complete                 # capture exit_code of the fast loop

**Smoke-testing a just-built binary: use the bare toolbox name.** `^$exe` and
`^_build/.../op` are gate-denied (computed head / path dispatch), but that
doesn't mean the fresh build is unreachable: the toolbox entry for `op` is a
symlink resolved through the host PATH **at exec time**, and host `op` is itself
a symlink into `_build`. So after `make build`, bare `^op --json "..."` *is* the
freshly built binary — no path needed, and nested op is sanctioned (bounded by
`OP_DEPTH`):

    make build | complete
    "describe this" | ^op --image foo.png --json | from json | get answer

General pattern for any artifact: give it a stable symlink on the host PATH and
its bare name a toolbox entry; then rebuild-and-run works from inside the
sandbox. Don't burn round-trips on path or computed-head workarounds — they are
denied by design.

## Modules (when the profile provides them)

If the tool description lists vetted modules, import them by **bare name**:

    use gpx.nu
    ... | gpx to-gpx | save -f out.gpx

The gate audits the module file with the same rules as your script (its
externals must be toolbox names too), so a passing `use` is safe by
construction. `use ./gpx.nu`, any path form, and unlisted files stay denied.

**Which profile am I under?** `$env.OP_NU_PROFILE` is the active profile name;
`$env.OP_NU_EXTERNALS` and `$env.OP_NU_MODULES` are comma-separated lists of
what it grants (`$env.OP_NU_MODULES | split row ","`). No need to go read
settings.json.

## Authoring .nu files: `nu-check`, then use on the next call

`nu-check` **parse-checks a file without running it** — `true` on success,
span-pointed errors with `--debug`. It's a builtin, so it passes the gate.
This is your fast feedback loop when writing a module: it catches
`export`/`const`/paren mistakes in seconds; don't paste the module body inline
to test it (top-level `export def` won't parse there anyway). One trap:

    nu-check --debug mod.nu                        # File not found (!)
    nu-check --debug ("mod.nu" | path expand)      # true

Your script runs from a temp file, and nu-check resolves a *bare* relative
path against the script's own directory — not the cwd. Always wrap the path
in `("..." | path expand)`.

Module dirs are rescanned and imported sources are gate-vetted fresh on every
tool call. The authoring loop is: write the module into a configured module dir,
run `nu-check --debug ("mod.nu" | path expand)` until clean, then in a
**separate next nu call** run `use mod.nu` and exercise it. A file created during
a call is not visible to that call's already-completed scan.

## Polars (when the profile provides it)

First line: `plugin use polars` (bare — op supplies the registry; every other
`plugin` subcommand is denied). Then the in-memory dataframe pattern:

    plugin use polars
    $rows | polars into-df
      | polars group-by loc | polars agg [ (polars col cert | polars mean) ]
      | polars collect | polars into-nu

`polars into-nu` at the end returns plain nu data to keep piping.

## Gate quick-reference (what gets denied)

- Any external not in the toolbox; **path-form dispatch** (`^/usr/bin/x`, `./x`,
  `/abs/cmd`) — always call a bare literal toolbox name.
- `http`, `run-external`, `exec`, `source`, and other network/exec/dispatch
  builtins. Delegate network work to a nested `op --json "..."` (see `op --help`).
- Computed/dynamic command names, including `^$cmd` and `^(...)`. Keep the head
  a literal.
- `use` of anything but `std` or a `.nu` file found in a configured module dir
  at that call; `plugin` subcommands other than `plugin use <listed name>`.

## Gate denials that are really *your* syntax errors

The gate parses the script **before** nu runs it, so a Nushell syntax error
surfaces as a gate block ("the gate could not parse part of this script …")
and you never see nu's own parse diagnostic. Don't rewrite working constructs
to appease it — find the typo. Causes seen in real runs, all agent typos:

- **Hyphenated positional params are invalid nu.** `def f [show-detail: bool]`
  doesn't parse — flags auto-underscore (`--show-detail` → `$show_detail`),
  positionals don't. Write `def f [show_detail: bool]`. Hyphenated *def names*
  (`detail-rows`) are fine.
- A stray `$` on a custom-command call: `($total-lines $ml)` parses as a
  variable, not a call — write `(total-lines $ml)`.
- An unbalanced paren, e.g. `print $"total: ($n)")`.

Related denial shapes that look like gate bugs but aren't:

- "external command `my-def` is not in the nu toolbox" for your own def:
  the def isn't declared in *this* script. Defs don't persist across nu tool
  calls — every script must carry its own defs.
- A `[gate] warning: reminder: a recent call was flagged: …` appended to a
  **successful** result (exit code 0) refers to an *earlier* denied call, not
  this one. The code that just ran is fine. It fires once per flagged call —
  if you see it again, a *new* call was flagged in between.

`print (glob "*" | first 3)` and other parenthesized pipelines as arguments
pass the gate fine — no need to let-bind first.

## Gotchas seen while dogfooding

- `open CLAUDE.md | lines` → `only_supports_this_input_type`. Fix: `open --raw`.
- `... | where item.content =~ ...` → `IncompatiblePathAccess`. After
  `enumerate` the column is `item`, not `item.content`.
- `dune build @check 2>&1 | to text` → gate block ("could not parse"). The `2>&1`
  is the culprit; use `| complete` or drop it.
- `ls /usr/bin/as*` errors when nothing matches; `glob /usr/bin/as*` returns `[]`.
