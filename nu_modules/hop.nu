# hop.nu — interactive back-and-forth over op sessions ("hop between turns").
#
# hop is the interactive loop op itself deliberately doesn't have: op stays a
# one-shot primitive, and this wrapper threads `sessionId` from each run's
# result line into the next call's --sessionId. The module exports `main`, so
# `use hop.nu` gives you a bare `hop` command:
#
#   use ~/ocaml/op/nu_modules/hop.nu
#   hop --context CLAUDE.md
#
# Input is read with reedline (`input --reedline`), so long lines and pastes
# wrap cleanly and up/down arrows walk prompt history (kept in ~/.op_global).
# A multi-line paste submits at its first newline — compose anything
# multi-line in the editor via :e instead.
#
# `@file/path` in a prompt attaches that file to the send via `op --context`.
# Stock nu runs all of it; inline Tab completion on the @token is the one
# extra. `use hop-reader.nu` (patched nu only — ~/nushell, branch
# completer-input) injects a reader that wires the completer in; without it
# Tab is inert and everything else works.
#
# Turns render live from `op --stream`, so there is no transcript command:
# to re-read an old session, run `pi-session-view (session file)` directly.
#
# The module is shaped like an Elm program — model, msg, update, view — with
# the one concession an interactive shell forces: update runs its effects (op
# calls, $EDITOR, pickers) inline instead of returning commands for a runtime
# to perform. Read it bottom-up: `main` is the runtime loop (read a line, make
# a message, fold it), `parse-line` is the whole command syntax, `update` is the
# whole behaviour, and everything above them is the view and the effects they
# lean on. New commands slot in as a `parse-line` arm plus an `update` arm.

use pi-session-view.nu

# ── view ─────────────────────────────────────────────────────────────────────
# Everything hop prints on its own behalf. op's turns are not rendered here:
# they stream through pi-session-view inside send-op.

def dim [text: string] { print $"(ansi dark_gray)($text)(ansi reset)" }

def err [text: string] { print $"(ansi red)error: ($text)(ansi reset)" }

# A flag and its value, or nothing when unset. The list-building counterpart of
# "add this only if it's set": every argument list here is a list of optional
# fragments, flattened once. Don't generalise it into a `when cond [items]` —
# Nushell evaluates arguments eagerly, so the items would be built before the
# condition is tested and blow up on the null they were guarding against.
# `if cond { [...] } else { [] }` is natively lazy and is the form to use
# whenever a fragment's *contents* depend on the thing being tested; `flag` is
# safe only because `value` arrives as a bare variable.
def flag [name: string, value: any]: nothing -> list<string> {
    if $value == null { [] } else { [$name ($value | into string)] }
}

# How full the context window is, coloured as a traffic light: green while
# there is room, orange approaching op's contextLimit (0.85 by default), red
# once a handoff is due. Empty when op reported no `context` — the model has no
# contextWindow in models.json, so there is nothing to measure against.
def context-gauge [res] {
    let c = $res | get -o context
    if $c == null { return "" }
    let pct = ($c.pct * 100 | math round)
    let colour = if $c.pct < 0.6 { "green" } else if $c.pct < 0.85 { "yellow" } else { "red" }
    $"(ansi $colour)ctx ($pct)%(ansi reset)"
}

# One dim line of run stats after each answer.
def status-line [res] {
    let stop = $res | get -o stopReason
    let t = $res | get -o tokens
    # `if/else []` rather than `flag`: each fragment interpolates the very value
    # it is testing for null. Fragments are lists, not single parts, because
    # turns/tools contributes two — flattening keeps the shape uniform where
    # compacting nulls would not.
    let parts = [
        (if ($res | get -o model) != null { [$res.model] } else { [] })
        (if $stop != null and $stop != "finished" { [$"stopped: ($stop)"] } else { [] })
        (if ($res | get -o turns) != null {
            [$"turns ($res.turns)" $"tools ($res.toolCalls)"]
        } else { [] })
        (if $t != null { [$"tokens ($t.input)+($t.cacheRead) in, ($t.output) out"] } else { [] })
    ] | flatten
    # The gauge carries its own colour, so it goes outside the dim wrapper.
    let gauge = context-gauge $res
    if ($parts | is-empty) { return $gauge }
    let dimmed = $"(ansi dark_gray)($parts | str join ' · ')(ansi reset)"
    if $gauge == "" { $dimmed } else { $"($dimmed) ($gauge)" }
}

# The footer after a send: op's error if any, the stats line, and the handoff
# nudge when the gauge trips. The answer itself already printed as it streamed
# (the final assistant entry), so the result contributes only this.
def show-result [res] {
    if ($res | get -o error) != null {
        err $res.error
        if ($res | get -o hint) != null { dim $res.hint }
    }
    print (status-line $res)
    if ($res | get -o stopReason) == "context_limit" {
        dim "context full — :h <goal> to hand off to a fresh session"
    }
}

# ── effects ──────────────────────────────────────────────────────────────────
# The world hop touches: op invocations and $EDITOR. Each returns plain data
# (or null for "nothing came of it") and leaves deciding what it means to
# update. The one licence: interactive pickers render their own chrome — the
# instruction line above an `input list` is part of the picker, not a verdict.

# hop just watched the user type this turn, so the session's own echo of it is
# noise in the live view.
def user-echo [e: record] {
    ($e | get -o type) == "message" and (($e | get -o message | get -o role) == "user")
}

# Run op --stream, rendering each session entry as it is appended, and return
# the run's final result record. The streamed lines are exactly the session
# file's, so pi-session-view's renderer serves them unchanged — the only
# difference from rendering a finished file is when they arrive. A hard crash
# that produces no result line (op normally emits an error envelope even on
# exit 1) becomes an error record of the same shape instead of throwing
# mid-chat.
#
# A fold rather than an `each` because the renderer needs one piece of memory:
# a toolResult names its call by id, so naming the file a read result took means
# carrying the calls seen so far. `main`'s file path gets that from a prepass;
# a live stream has no lookahead, so it accumulates here.
def send-op [args: list<string>] {
    let final = (
        ^op --stream ...$args
        | lines
        | reduce --fold {calls: {}, result: null} {|l, st|
            let e = try { $l | from json } catch { null }
            if $e == null {
                $st
            } else if ($e | get -o type) == "result" {
                $st | merge {result: $e}
            } else {
                if not (user-echo $e) { pi-session-view render-entry $e $st.calls }
                $st | merge {calls: ($st.calls | merge (pi-session-view calls-of $e))}
            }
        }
    )
    if $final.result == null {
        {isError: true, error: "op produced no result line"}
    } else {
        $final.result
    }
}

# Open $EDITOR (default hx) on the chat buffer. True iff the user saved:
# quitting without writing leaves the mtime alone, which we treat as cancel.
def edit-buffer [buf: path] {
    let before = ls $buf | get 0.modified
    let editor = $env.EDITOR? | default "hx" | split row " "
    run-external ($editor | first) ...($editor | skip 1) $buf
    (ls $buf | get 0.modified) != $before
}

# The latest sessions for this cwd (op scopes the store per directory), newest
# first, each row carrying its message list as a nested table — the structure
# :l hands to `explore`. Built from two op listings, so ids here are exactly
# what --sessionId / --parentId accept.
def session-tree [limit: int] {
    ^op --json --list sessions --limit $limit
    | from json
    | get sessions
    | sort-by timestamp --reverse
    | each {|s|
        # Every message, not op's default last 25: :l exists to reach *back*
        # into a session — a long one truncated to its tail hides exactly the
        # early turn you came to fork from. The session's own entry count is a
        # safe upper bound (messages are a subset of entries).
        let msgs = (
            try {
                ^op --json --list messages --sessionId $s.id --limit $s.count
                | from json
                | get messages
            } catch { [] }
        )
        {
            id: $s.id
            when: ($s.timestamp | into datetime | date to-timezone local | format date "%m-%d %H:%M")
            n: $s.count
            summary: ($s | get -o summary | default "")
            messages: ($msgs | select -o id role preview isFork)
        }
    }
}

# Where in $session the next send should attach: its branch tip, asking when
# there is more than one. Picking up a session is the moment that choice has to
# be made — op cannot guess between tips, so without this hop would just hand
# back the ambiguity error. Tips are fetched here rather than in `session-tree`
# because that would double :l's op calls to answer a question about one row.
# Null means carry on without a tip: an unknown session, or a cancelled pick.
def pick-tip [session: string] {
    let tips = (
        # Same reason as session-tree's message limit: a tip hidden by the
        # default 25 is a branch you cannot continue.
        try {
            ^op --json --list branches --sessionId $session --limit 1000
            | from json
            | get branches
        } catch { [] }
    )
    # One tip is not a choice, and no tips means op will have its own say.
    if ($tips | length) <= 1 { return ($tips | get -o 0.tip) }
    dim $"($tips | length) branches — pick the one to continue, or Esc to cancel"
    let choice = (
        $tips
        | each {|t| {
            when: ($t.when | into datetime | date to-timezone local | format date "%m-%d %H:%M")
            role: $t.role
            tail: $t.preview
            tip: $t.tip
        }}
        | input list --display {|it| $"($it.when)  ($it.role)  ($it.tail)" }
    )
    $choice | get -o tip
}

# Ask op for a handoff prompt: a brief that lets a *fresh* session carry on this
# work with an empty context (op --handoff reads the session and never appends
# to it). $parent names the branch to read on a branched session, exactly as it
# does for sends. Returns op's result envelope — {handoffPrompt} on success,
# {isError, error} on failure — or null when stdout held no JSON at all.
# `complete` rather than try/catch: op exits 1 with its error in the JSON
# envelope on stdout, and a catch would discard that envelope unread.
def handoff-draft [session: string, parent: any, goal: string] {
    let args = [
        ["--handoff" "--json" "--sessionId" $session]
        (flag "--parentId" $parent)
        (if $goal == "" { [] } else { [$goal] })
    ] | flatten
    let out = (^op ...$args | complete)
    try { $out.stdout | from json } catch { null }
}

# Every configured provider/model pair, as the exact strings --model accepts.
def list-models [] {
    try {
        ^op --json --list models --limit 1000
        | from json
        | get models
        | each {|m| $"($m.provider)/($m.model)" }
    } catch { [] }
}

# Pick from op's configured models, showing the current override in the prompt.
# Null on Esc, as `pick-tip` is null on cancel — and null too when there are no
# models to offer, which the picker reports itself under the same licence
# `pick-tip` uses to print its own instruction line.
def pick-model [current: any] {
    let models = (list-models)
    if ($models | is-empty) {
        dim "no models found — check ~/.op_global/agent/models.json"
        return null
    }
    $models | input list --fuzzy $"model (($current | default 'op default')) >"
}

# ── model ────────────────────────────────────────────────────────────────────
# hop's state is one record threaded through the loop — everything a turn can
# change, and nothing else:
#   session  the session the next send attaches to (null: start fresh)
#   parent   the entry within it — branch tip or fork point (null: op decides)
#   model    the :m provider/model override (null: op's default)
#   errors   consecutive input failures (see `interrupted` in update)
#   done     set when the loop should stop
# The immutable counterpart is `cfg`, built once in main: the buffer and
# history paths, the --profile/--context/--max-steps flags, and the @path
# completer. update takes both; only state comes back.

def init [session: any, parent: any, model: any] {
    {session: $session, parent: $parent, model: $model, errors: 0, done: false}
}

# ── msg ──────────────────────────────────────────────────────────────────────
# One typed line → one message: {msg: kind, arg: rest-of-line}. Pure, so the
# whole command syntax sits in this one table and update only dispatches on
# kind. An unrecognised :word falls through as a prompt like any other text —
# op may make sense of it even when hop doesn't. (`parse-line`, not `parse`:
# a def named `parse` would shadow the builtin update-send scans @tokens with.)
def parse-line [raw: string] {
    let line = $raw | str trim
    let cmd = $line | split row " " | first
    let arg = $line | str substring ($cmd | str length).. | str trim
    match $cmd {
        "" => ({msg: "nothing", arg: ""})
        ":q" => ({msg: "quit", arg: ""})
        ":n" => ({msg: "new", arg: ""})
        ":l" => ({msg: "browse", arg: $arg})
        ":m" => ({msg: "model", arg: $arg})
        ":h" => ({msg: "handoff", arg: $arg})
        ":e" => ({msg: "edit", arg: $arg})
        _ => ({msg: "send", arg: $line})
    }
}

# ── update ───────────────────────────────────────────────────────────────────
# One function per message kind, each state → state. The three send paths
# (typed line, :e buffer, :h fork) converge on run-send, which owns the whole
# prompt-to-op translation.

# :l — browse this cwd's sessions in `explore`, and pick where the next send
# attaches: a session id resumes it (asking which branch when it has several),
# a message id resumes its session forked at that entry.
def update-browse [state: record, arg: string] {
    let limit = if $arg == "" { 15 } else { try { $arg | into int } catch { 15 } }
    let tree = (session-tree $limit)
    if ($tree | is-empty) {
        dim "no sessions for this directory yet"
        return $state
    }
    dim "Enter cursors in · Esc backs out / just browses · :q picks the id under the cursor"
    let picked = ($tree | explore --peek)
    # Esc quits from browse mode and peeks the whole view (a table or record)
    # — only a cursor-picked string cell can be an id.
    if ($picked | describe) != "string" { return $state }
    if $picked in ($tree | get id) {
        dim $"next send resumes session ($picked)"
        return ($state | merge {session: $picked, parent: (pick-tip $picked)})
    }
    let owner = ($tree | where {|s| $picked in ($s.messages | get -o id | default [])})
    if ($owner | is-empty) {
        dim $"'($picked)' is not a session or message id — nothing picked"
        return $state
    }
    dim $"next send resumes session ($owner.0.id) forked at ($picked)"
    $state | merge {session: $owner.0.id, parent: $picked}
}

# :m — with an argument, set the override directly; bare, fetch op's model
# list and pick from a fuzzy filter (type to narrow, Enter sets, Esc keeps
# whatever was set before).
def update-model [state: record, arg: string] {
    let choice = if $arg != "" { $arg } else { pick-model $state.model }
    if $choice == null {
        dim "model unchanged"
        return $state
    }
    dim $"model set to ($choice)"
    $state | merge {model: $choice}
}

# :h — draft a handoff brief from this session, review it in $EDITOR, and send
# it as the opening prompt of a fresh session linked back to this one.
def update-handoff [state: record, cfg: record, goal: string] {
    if $state.session == null {
        dim "nothing to hand off yet"
        return $state
    }
    let res = (handoff-draft $state.session $state.parent $goal)
    if $res == null {
        err "handoff produced no result"
        return $state
    }
    if ($res | get -o isError) == true {
        err ($res | get -o error | default "handoff failed")
        return $state
    }
    $res.handoffPrompt | save --force $cfg.buf
    # Same convention as :e — the draft is reviewed and edited before it is
    # sent, so nothing is carried over that the user did not read.
    if not (edit-buffer $cfg.buf) {
        dim "draft not saved — nothing sent"
        return $state
    }
    run-send $state $cfg {prompt: ["--prompt" $cfg.buf], from: $state.session, contexts: []}
}

# :e — compose the prompt in $EDITOR, optionally seeding the buffer from
# skills/<skill>.md first.
def update-edit [state: record, cfg: record, skill: string] {
    if $skill != "" {
        let seed = $"skills/($skill).md"
        if not ($seed | path exists) {
            dim $"no skill file ($seed)"
            return $state
        }
        open --raw $seed | save --force $cfg.buf
    } else if not ($cfg.buf | path exists) {
        touch $cfg.buf
    }
    if not (edit-buffer $cfg.buf) {
        dim "buffer not saved — nothing sent"
        return $state
    }
    run-send $state $cfg {prompt: ["--prompt" $cfg.buf], from: null, contexts: []}
}

# A plain line is a prompt. @path mentions attach files to the send: every
# whitespace-delimited @token naming an existing file rides along as a
# --context flag, while the mention itself stays in the prompt so the model
# sees what the files were attached *for*. Tokens that are not files — op's
# @skill names above all — pass through untouched for op to interpret. Buffer
# sends (:e/:h) are not scanned: their text was composed, not typed.
def update-send [state: record, cfg: record, line: string] {
    let at_files = (
        $line
        | parse -r '(?:^|\s)@(\S+)'
        | get capture0
        | uniq
        | where {|p| ($p | path type) == "file" }
    )
    run-send $state $cfg {prompt: [$line], from: null, contexts: $at_files}
}

# Where this send attaches, as op flags. Three genuinely different shapes: a
# fork (--fromSession, a fresh session linked back to its source) and a
# first/:n send are both *fresh* sessions, and a fresh session needs cfg's
# context file injected — a resume already carries it in the session's first
# user message.
def attach-args [state: record, cfg: record, from: any] {
    let ctx = if $cfg.context == null { [] } else { ["--context" $cfg.context] }
    if $from != null {
        ["--fromSession" $from] ++ $ctx
    } else if $state.session != null {
        let at = if $state.parent == null { [] } else { ["--parentId" $state.parent] }
        ["--sessionId" $state.session] ++ $at
    } else {
        $ctx
    }
}

# The one place a prompt becomes an op invocation. $req is what varies between
# the send paths: `prompt` (a positional prompt, or --prompt FILE so
# multi-line content never round-trips through shell quoting), `from`
# (--fromSession for the handoff fork), `contexts` (@path attachments).
def run-send [state: record, cfg: record, req: record] {
    # One fragment per source of arguments, flattened once — `flatten` is a
    # single level, hence the inner one on the repeated --context pairs.
    let args = [
        ($req.contexts | each {|f| ["--context" $f] } | flatten)
        (flag "--model" $state.model)
        (flag "--profile" $cfg.profile)
        (flag "--tools" $cfg.tools)
        (flag "--max-steps" $cfg.max_steps)
        (attach-args $state $cfg $req.from)
        $req.prompt
    ] | flatten
    let res = (send-op $args)
    show-result $res
    # Follow the leaf, not just the session. `parentId` in the result is where
    # this run left off, so the next send names it explicitly and stays on
    # this branch. Resuming by --sessionId alone would work only until the
    # session forked: a branched session has several leaves and op rightly
    # refuses to guess between them, which used to strand hop on any session
    # it had ever branched. A send that produced no result line (op killed
    # mid-turn) leaves parent on the last known-good entry, so continuing
    # re-forks from there rather than dead-ending. session advances even on
    # isError: op logs the failed turn too, and resuming past it is the
    # documented recovery path.
    $state | merge {
        parent: ($res | get -o parentId | default $state.parent)
        session: ($res | get -o sessionId | default $state.session)
    }
}

# Fold one message into the state. Every arm returns the next state; effects
# happen inline in its update-* helper. Any message that is not `interrupted`
# proves someone is typing, so it clears the failure count first.
def update [state: record, cfg: record, msg: record] {
    let state = if $msg.msg == "interrupted" { $state } else { $state | merge {errors: 0} }
    match $msg.msg {
        "nothing" => $state
        "quit" => ($state | merge {done: true})
        "interrupted" => {
            # Ctrl-c lands here and must not quit — it is the terminal's copy
            # key, and quitting on it is how a chat gets lost mid-copy. But a
            # non-interactive stdin fails forever, so a short run of failures
            # with nothing typed in between means no one is there.
            let n = $state.errors + 1
            if $n > 20 { dim "input keeps failing — quitting" }
            $state | merge {errors: $n, done: ($n > 20)}
        }
        "new" => {
            dim "next send starts a fresh session"
            $state | merge {session: null, parent: null}
        }
        "browse" => (update-browse $state $msg.arg)
        "model" => (update-model $state $msg.arg)
        "handoff" => (update-handoff $state $cfg $msg.arg)
        "edit" => (update-edit $state $cfg $msg.arg)
        "send" => (update-send $state $cfg $msg.arg)
        # A match with no matching arm returns nothing, so without this a
        # `parse-line` kind added without its counterpart here would set $state
        # to null and die at $state.done with a cell-path error naming nothing.
        _ => { err $"no handler for ($msg.msg)"; $state }
    }
}

# ── runtime ──────────────────────────────────────────────────────────────────

# Interactive back-and-forth over one op session. Plain text sends, with
# `@file/path` tokens (Tab-completed) attaching those files as --context;
# commands:
#   :e          compose the prompt in $EDITOR (:wq sends; quit unsaved cancels)
#   :e <skill>  same, but seed the buffer from skills/<skill>.md first
#   :h [goal]   hand off: op drafts a prompt carrying this session's work into a
#               fresh one (goal, progress, decisions, next steps, files touched);
#               the draft opens in $EDITOR for review, and saving sends it as a
#               new session linked back to this one
#   :l [n]      browse the last n (default 15) sessions of this cwd in
#               `explore`: drill into a row's messages with Enter, Esc backs
#               out / quits. Quitting with `:q` picks the cell under the
#               cursor: a session id resumes that session — asking which
#               branch when it has several — and a message id resumes its
#               session forked at that entry (--parentId)
#   :m [model]  set the provider/model override for subsequent sends; bare :m
#               lists op's models in a fuzzy picker (type to filter, Enter
#               sets, Esc keeps the current override)
#   :n          next send starts a fresh session (re-injecting --context)
#   :q          quit (ctrl-d also works; ctrl-c only clears the current line,
#               so it stays free for copying out of the terminal)
# The editor buffer is one temp file kept for the whole chat, so it doubles as
# a working pad: the last prompt is still there to revise, undo history intact.
export def main [
    --model (-m): string    # provider/model override for every send
    --profile: string       # capability profile (a name from nuProfiles)
    --tools: string         # tool set for every send, e.g. read,glob — or
                            # "none" for a pure chat (a profile with a tools
                            # list, e.g. --profile chat, does this too)
    --session: string       # pick up an existing session instead of starting fresh
    --parent: string        # entry to pick --session up at, skipping the branch
                            # prompt: a tip from `op --list branches` continues
                            # that branch, any other entry id forks there. Only
                            # the first send needs it — after that hop follows
                            # its own leaf
    --context (-c): string  # context file (e.g. CLAUDE.md), injected with the
                            # first send only — it persists in the session, so
                            # every later turn inherits it on resume
    --max-steps: int        # step cap per send
] {
    let cfg = {
        buf: ($nu.temp-dir | path join $"hop-(random uuid).md")
        history: ($nu.home-dir | path join ".op_global" "hop_history.txt")
        profile: $profile
        tools: $tools
        context: $context
        max_steps: $max_steps
        # Inline completion for @path tokens: the trailing word before the
        # cursor, when it starts with @, completes as a file path through the
        # engine's own completer, keeping the @ prefix. `--type path` matters:
        # without it a bare token sits in command position and completes PATH
        # binaries instead of files. Anything else completes to nothing, so
        # Tab stays inert in plain prose.
        completer: {|buf, pos|
            let before = $buf | str substring 0..<$pos
            let tok = $before | split row -r '\s+' | last
            if ($tok | str starts-with "@") {
                let start = ($before | str length) - ($tok | str length)
                ($tok | str substring 1..) | commandline complete --type path
                | each {|c| {value: $"@($c)", start: $start, end: $pos} }
            } else { [] }
        }
    }
    dim "@path TAB -> attach context · :e [file] -> edit prompt · :h [goal] -> handoff · :l [n] -> browse sessions · :m [model] -> pick/set model · :n -> new thread · :q -> quit"
    # Same choice :l makes on a pick, made once for --session: a branched
    # session cannot be resumed without naming a tip, and being asked beats
    # being told so by the first send. Resolved before `init` so that `init`
    # stays the one place a state record is constructed.
    let start_parent = if $session != null and $parent == null {
        pick-tip $session
    } else {
        $parent
    }
    mut state = (init $session $start_parent $model)
    # The reader is injectable so the fork-only syntax never parses here:
    # hop-reader.nu (patched nu only) sets $env.HOP_READER to a reader that
    # hands $completer to `input --completer`. Without it, stock reedline —
    # everything works except @path Tab completion. Chosen with `if`, not
    # `| default`: default *calls* a closure to compute the value.
    let fallback = {|history, completer|
        input --reedline --history-file $history "hop> "
    }
    let reader = if $env.HOP_READER? == null { $fallback } else { $env.HOP_READER }
    loop {
        # reedline: ctrl-c throws, ctrl-d returns null. The catch value is a
        # record so a thrown line is distinguishable from a typed empty one —
        # only the former becomes an `interrupted` message.
        let raw = (try { do $reader $cfg.history $cfg.completer } catch { {interrupted: true} })
        let msg = if $raw == null {
            {msg: "quit", arg: ""}
        } else if ($raw | describe) != "string" {
            {msg: "interrupted", arg: ""}
        } else {
            parse-line $raw
        }
        $state = (update $state $cfg $msg)
        if $state.done { break }
    }
    # Name the leaf as well: by now the session may have branches (a :l fork,
    # or an earlier hop), and --session alone would be ambiguous on the way
    # back in — the same dead end $parent exists to avoid mid-chat.
    if $state.session != null {
        let at = if $state.parent == null { "" } else { $" --parent ($state.parent)" }
        dim $"resume: hop --session ($state.session)($at)"
    }
}
