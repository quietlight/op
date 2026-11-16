# pi session format

op logs every run as a session file in the same jsonl format pi uses, so the
two tools can read each other's sessions. This document describes the format
(version 3) and exactly which subset op writes today. The machine-readable
companion is [pi-session.schema.json](pi-session.schema.json); the reference
implementation is pi's `session-manager.ts` (types) and `types.ts` in
`pi-ai` (message shapes).

## File location and naming

```
<root>/--<encoded cwd>--/<timestamp>_<session id>.jsonl
```

- root: `~/.op_global/sessions` for op, `~/.pi/agent/sessions` for pi.
- encoded cwd: the absolute working directory with the leading `/` stripped
  and every `/`, `\` and `:` replaced by `-`, wrapped in `--`. Example:
  `/home/user/ocaml/op` → `--home-user-ocaml-op--`.
- timestamp: the header timestamp with `:` and `.` replaced by `-`
  (`2026-06-10T01-46-04-743Z`).
- session id: a UUIDv7, so file names sort chronologically.

## File structure

One JSON object per line. The first line is the **header**; every other
line is an **entry**.

```json
{"type":"session","version":3,"id":"019eaf35-…","timestamp":"2026-06-10T01:46:04.743Z","cwd":"/home/user/ocaml/op"}
{"type":"model_change","id":"38d94815","parentId":null,"timestamp":"…","provider":"zai","modelId":"glm-5.1"}
{"type":"thinking_level_change","id":"dbe5dcbf","parentId":"38d94815","timestamp":"…","thinkingLevel":"off"}
{"type":"message","id":"0bbbc28d","parentId":"dbe5dcbf","timestamp":"…","message":{"role":"user","content":[{"type":"text","text":"…"}],"timestamp":1781055983852}}
{"type":"message","id":"4de4c1f0","parentId":"0bbbc28d","timestamp":"…","message":{"role":"assistant","content":[…],"api":"openai-completions","provider":"zai","model":"glm-5.1","usage":{…},"stopReason":"toolUse","timestamp":…}}
{"type":"message","id":"a35bee2e","parentId":"4de4c1f0","timestamp":"…","message":{"role":"toolResult","toolCallId":"call_…","toolName":"read","content":[{"type":"text","text":"…"}],"isError":false,"timestamp":…}}
```

The header may carry `parentSession` (path to the file this session was
forked from). It is *not* part of the entry tree. op writes it for
`op --fromSession ID` — a fresh session that continues an older one's work from
a handoff prompt (see `op --help`, CONTEXT AND HANDOFF) — and for the gate log
below.

## The entry tree

Every entry has:

| field       | meaning                                                       |
|-------------|---------------------------------------------------------------|
| `type`      | discriminator (see below)                                      |
| `id`        | unique within the session; 8 hex chars (a full UUID only on collision) |
| `parentId`  | `id` of the parent entry, `null` for the root entry            |
| `timestamp` | ISO-8601 UTC with milliseconds                                 |

New entries are always appended as a child of the current **leaf**, so an
ordinary session is a linear chain in file order. The tree shape exists for
**forking**: appending an entry whose `parentId` is some earlier entry starts
a new branch (the file remains append-only; sibling branches simply share a
parent). This is what lets op re-enter a conversation at any point:
`op --sessionId <session-id>` continues from the leaf, and adding
`--parentId <entry-id>` branches from that entry (abandoning a branch where the
agent went awry). Resuming is keyed by the session id — the entry id alone is a
message *within* a session, not a session selector.

### Rebuilding context from a leaf

To construct the LLM context for a given leaf entry (pi's
`buildSessionContext`):

1. Walk `parentId` links from the leaf to the root and reverse — that path
   is the timeline; entries on other branches are invisible.
2. The latest `model_change` / `thinking_level_change` on the path determine
   the active model and thinking level (an assistant message also implies its
   own `provider`/`model`).
3. If the path contains a `compaction` entry, take the **latest** one: emit
   its `summary` (as a synthetic message) first, then the path entries from
   `firstKeptEntryId` up to the compaction, then everything after it.
   Everything older than `firstKeptEntryId` is dropped.
4. Otherwise emit every `message` on the path in order (`custom_message`
   entries become user messages; `branch_summary` entries become synthetic
   messages).

Note that the system prompt is **not** stored in the session; the consumer
supplies it.

## Entry types

| type                    | written by op | purpose                                                      |
|-------------------------|---------------|--------------------------------------------------------------|
| `message`               | yes           | one conversation message (see roles below)                   |
| `model_change`          | yes (at start)| provider/model switch                                        |
| `thinking_level_change` | yes (at start, always `"off"`) | reasoning-effort switch                     |
| `compaction`            | no            | summary replacing history older than `firstKeptEntryId`      |
| `branch_summary`        | no            | summary of an abandoned branch                               |
| `custom`                | no            | extension state, never sent to the LLM                       |
| `custom_message`        | no            | extension content injected into LLM context                  |
| `label`                 | no            | user bookmark on another entry                               |
| `session_info`          | no            | session metadata (display name)                              |

Field-level detail for all of these lives in the JSON schema.

## Message roles

A `message` entry wraps one `message` object discriminated by `role`. The
`timestamp` inside the message is Unix **milliseconds** (the entry-level
timestamp is an ISO string).

- **`user`** — `content` is a string or a list of `text` / `image` blocks.
  op writes the prompt as a `text` block followed by any image attachments.
- **`assistant`** — `content` is a list of `text` / `thinking` / `toolCall`
  blocks, plus `api` (wire protocol, e.g. `anthropic-messages`), `provider`,
  `model`, `usage`, `stopReason` (`stop` | `length` | `toolUse` | `error` |
  `aborted`) and optionally `responseModel`, `responseId`, `errorMessage`,
  `diagnostics`.
- **`toolResult`** — `toolCallId` matching the `toolCall` block it answers,
  `toolName`, `content` (list of `text` / `image` blocks), `isError`, and
  optionally tool-specific `details`.

pi extensions define further roles (`bashExecution`, `custom`,
`branchSummary`, `compactionSummary`); op neither writes nor needs them.

## What op writes today (and deviations)

A fresh run writes: header → `model_change` → `thinking_level_change`
(`"off"`) → the user prompt → alternating assistant / toolResult messages,
flushed per entry (a crashed run still leaves a valid log). A run with
`--sessionId <id>` instead *resumes* that session file: the leaf is the
session's last entry, or `--parentId <entry-id>` when given. It appends a
`model_change` (the model actually used this run) and the new turn as children
of the chosen leaf — so a parent that already has children gains a sibling
branch, and the file holds a tree. History for any leaf is rebuilt with
`Session.load_messages` (walk parentId links to the root).

Known simplifications, all schema-valid:

- `usage` carries real `input`/`output`/`cacheRead`/`cacheWrite` counts when the
  provider reports them (zeros otherwise); `totalTokens` is `input + output`,
  and all `cost` fields are 0 because op does not know prices. `load_messages`
  reads these back onto the replayed assistant turns (an all-zero object comes
  back as no usage), which is how a resumed run knows how full the context
  window already is before it makes its first call.
- `stopReason` is inferred: `toolUse` when the turn carries tool calls,
  `stop` otherwise (`length` / `error` / `aborted` are never produced).
- Assistant messages carry a `thinking` block (with `thinkingSignature`)
  when the provider returned reasoning; `responseId` and `diagnostics` are
  never written. `details` on tool results is currently written by no built-in
  tool. Historical op sessions and pi's `examples/extensions/todo.ts` may carry
  a todo state snapshot there (`{action, todos, nextId, error?}`); op continues
  to parse and preserve the optional pi-format field.
- Compaction, branch summaries, labels, and session naming are not
  produced, and `load_messages` *skips* compaction/branch_summary entries
  it encounters in foreign (pi) sessions rather than honouring them. op's
  answer to a full context window is `--handoff`, which writes no entry at all:
  it reads a session and emits a prompt for a *new* one, linked back through
  the header's `parentSession`.

## The live stream (`--stream`)

Under `--stream` op writes the very lines described above to **stdout** as they
are appended to the session file, one JSON object per line. The stream *is* this
format: a consumer that can render a session file
can render a live run with no extra vocabulary (unlike pi's `--mode json`, which
defines a separate agent-event model). This is what `hop` reads.

op's *default* stdout is not this — it is the human transcript that
`lib/render.ml` produces from these same entries, entry by entry as they are
written (`--json` is the third mode: the result envelope alone). All three views
are the same run through the same sink; only the formatting differs, so nothing
in this document changes with the flag.

- A fresh run streams the `session` header first, then every entry.
- A resumed run streams only its new entries — starting with the
  `model_change` — since the consumer already knows the id it passed to
  `--sessionId`.
- The **final** line is not a session entry: it is the `--json` result envelope
  (`answer`, `sessionId`, `parentId`, `model`, `tokens`, run metadata,
  `stopReason`, `isError`) with a `"type": "result"` field prepended, so it is
  distinguishable from the entry lines without reassembling `answer`. A hard
  failure is the tagged error envelope,
  `{"type":"result","error":…,"isError":true}`. Exit codes match `--json`.
- Gate-judge entries (below) go to their own file and are **never** streamed.
- If session logging is impossible (e.g. `HOME` unset), op warns on stderr and
  the stream degrades to the result line alone — streaming never blocks a run.

## Gate judge logs

Runs whose gate consults the LLM judge (see `lib/gate_judge.mli`) also write an
**audit log in this same format** under `~/.op_global/gate/--<encoded cwd>--/`
— a sibling root to `sessions`, same file naming, same header/entry schema. The
file is created lazily on the first judge call, so the common zero-judge-call
run leaves nothing behind; when one exists, its path is printed on stderr
(`gate log: …`) at the end of the run.

Shape: header (`parentSession` points at the run's main session file) →
`model_change` (the configured `gateModel`) → then, per judge call:

- a `user` message — the exact JSON payload the judge model received
  (`{"recent": [...], "command": "..."}`); the judge's system prompt is not
  stored, per pi convention (it is a constant in the binary);
- an `assistant` message — the raw response with real token usage, absent when
  the transport failed;
- a `custom` entry, `customType` `"gate_verdict"`, whose `data` records the
  final parsed verdict: `verdict` (`allow`/`warn`/`deny`/`code_red`), `reason`
  (the verdict's reason), `routedBecause` (why the deterministic rules routed
  the command to the judge), and `command`. The verdict is recorded explicitly
  because it can differ from the raw response — unparseable output, unknown
  verdicts, and transport errors all coerce to deny.

This is an audit trail wearing session clothes: consecutive judge calls are
independent conversations, not multi-turn history, so resuming a gate log is
meaningless and `op --list` deliberately ignores these files. Browse them raw
or with `nu_modules/pi-session-view.nu`. The intended uses are tracking down
judge errors and miss-allows, and spotting commands that recur often enough to
deserve a deterministic allowlist entry.
