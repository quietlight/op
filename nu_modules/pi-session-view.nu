# pi-session-view.nu — render an op/pi agent-session .jsonl log as a
# human-readable transcript.
#
# Usage:
#   use pi-session-view.nu
#   pi-session-view /path/to/session.jsonl
#
# The session format is documented in op's docs/pi-session-format.md.
# Each line of the file is a JSON object; the first is the "session" header
# and the rest are entries. This module renders the header, model changes,
# and message entries (user / assistant / toolResult). Entry types it does
# not know how to render are silently skipped, and missing optional fields are
# tolerated so a partial/crashed log still displays.
#
# KEPT AS AN EXAMPLE. Nothing in op calls this module any more: `op --render`
# replays a session file (or a piped `op --stream`) through op's own renderer,
# `lib/render.ml`, and hop uses that. This file stays because it is good
# Nushell to read and to borrow from — it was the strongest entry in a
# which-model-writes-the-best-nu comparison — and because it still works on
# its own for any op or pi session log.
#
# It was written as the twin of `lib/render.ml`: the constants below are that
# module's line budgets and the layout is its `head` (a blank line before each
# USER/ASSISTANT header and nowhere else, `→`/`←` heads with the tool name in a
# five-wide column, bodies indented four). It is no longer kept in step, so
# expect it to drift from op's transcript as render.ml changes. One difference
# was always deliberate: this module keeps a checklist renderer for `todo`
# entries (legacy op sessions and pi's todo.ts carry list state in the
# toolResult `details`), where render.ml shows them through its generic
# unknown-tool path.

# --- limits (lib/render.ml's, as of 2026-11) ------------------------------------

const PREVIEW_LINES = 5
const NU_PREVIEW_LINES = 10
const ERROR_PREVIEW_LINES = 15
const SCRIPT_LINES = 15
const THINKING_LINES = 5
const TODO_ITEMS = 5
const VALUE_CHARS = 60
const EDIT_CHARS = 40
const LINE_CHARS = 200
const INDENT = "    "

# nu's exit footer, as lib/nu.ml writes it: "%s\n(exit code: %d)".
const EXIT_PREFIX = "(exit code: "

# --- helpers ---------------------------------------------------------------

def is-list [v]: nothing -> bool {
    let d = ($v | describe)
    ($d | str starts-with "list") or ($d | str starts-with "table")
}

# Cut to [n] characters, marking the cut. Chars, not bytes, throughout — nu's
# `str substring` indexes by char, and counting one way while cutting the other
# is how you saw a codepoint in half. render.ml counts bytes and then backs off
# to a boundary; the two agree on ASCII and differ only in where they land on a
# non-ASCII line that was being truncated anyway.
def clip [n: int, s: string]: nothing -> string {
    if ($s | str length --chars) > $n { ($s | str substring 0..<$n) + "…" } else { $s }
}

# Collapse to one line, for a value going into a head.
def one-line [s: string]: nothing -> string {
    $s | str replace -a "\n" " " | str trim
}

# A JSON argument as the text a human wants to see: bare for a string, its
# compact encoding otherwise.
def text-of [v]: nothing -> string {
    if ($v | describe) == "string" { $v } else { $v | to json -r }
}

# Extract and join all `text` blocks from a message `content` value.
# `content` is normally a list/table of blocks, but may be a bare string.
def block-text [content]: nothing -> string {
    if (is-list $content) {
        $content
        | where {|b| ($b | get -o type) == "text"}
        | get -o text
        | compact
        | str join "\n"
    } else if (($content | describe) == "string") {
        $content
    } else {
        ($content | to text)
    }
}

# Paths are absolute in tool arguments; against a known cwd the run reads as
# the project it is in rather than as a column of $HOME. The root is the
# session header's cwd (set by `main`), falling back to the working directory —
# which is the right answer for hop, since op runs in it. A literal "/" like
# render.ml and glob.ml, never $nu.path-sep: these are the paths the tools saw.
def rel [path: string]: nothing -> string {
    let cwd = ($env.PI_SESSION_CWD? | default $env.PWD)
    if ($cwd != "") and ($path | str starts-with ($cwd + "/")) {
        $path | str substring (($cwd | str length --chars) + 1)..
    } else {
        $path
    }
}

def human-bytes [n: int]: nothing -> string {
    if $n < 1024 {
        $"($n) B"
    } else if $n < 1048576 {
        $"(($n / 1024.0) | into string --decimals 1) kB"
    } else {
        $"(($n / 1048576.0) | into string --decimals 1) MB"
    }
}

# The `→`/`←` head: name padded to a five-wide column so arguments line up down
# the run, then the padding trimmed back off so a call with nothing after its
# name (a nu script at the default timeout) leaves no trailing whitespace.
def head-line [arrow: string, name: string, rest: string]: nothing -> string {
    let padded = ($name | fill --alignment left --width 5)
    $"  (ansi magenta)($arrow)(ansi reset) ($padded) ($rest)" | str trim --right
}

def print-body [text: string, limit: int] {
    let ls = ($text | lines)
    for l in ($ls | first $limit) {
        print $"($INDENT)(ansi default_dimmed)(clip $LINE_CHARS $l)(ansi reset)"
    }
    if ($ls | length) > $limit {
        print $"(ansi default_dimmed)($INDENT)…  ($ls | length) lines total(ansi reset)"
    }
}

# --- tool calls ------------------------------------------------------------

# read's slice, when it asked for one. A limit with no offset still starts at
# line 1, so both forms name a range.
def read-range [args]: nothing -> string {
    let offset = ($args | get -o offset)
    let limit = ($args | get -o limit)
    if ($offset == null) and ($limit == null) {
        ""
    } else if $limit != null {
        let o = (if $offset == null { 1 } else { $offset })
        $"  lines ($o)-($o + ([$limit 1] | math max) - 1)"
    } else {
        $"  from line ($offset)"
    }
}

def edit-summary [args]: nothing -> string {
    let old = (clip $EDIT_CHARS (one-line ($args | get -o old_string | default "")))
    let new = (clip $EDIT_CHARS (one-line ($args | get -o new_string | default "")))
    let all = (if ($args | get -o replace_all) == true { $"  (ansi default_dimmed)\(all\)(ansi reset)" } else { "" })
    $"(rel ($args | get -o path | default "?"))  ($old) → ($new)($all)"
}

def todo-summary [args]: nothing -> string {
    let action = ($args | get -o action | default "?")
    let text = ($args | get -o text)
    let id = ($args | get -o id)
    if ($action == "add") and ($text != null) {
        $"add (clip $VALUE_CHARS (one-line $text))"
    } else if ($action == "toggle") and ($id != null) {
        $"toggle #($id)"
    } else {
        $action
    }
}

# The fallback for a tool this renderer does not know, so a new tool degrades
# to something readable instead of nothing.
def arg-summary [args]: nothing -> string {
    if (($args | describe) | str starts-with "record") {
        $args
        | transpose k v
        | each {|r| $"($r.k)=(clip $VALUE_CHARS (one-line (text-of $r.v)))" }
        | str join ", "
    } else if (($args | describe) == "nothing") {
        ""
    } else {
        (one-line ($args | to json -r))
    }
}

# The nu script itself, indented under the call — the single most useful thing
# to see in a run, and the one thing a one-line argument summary destroys.
def print-script [args] {
    let ls = ($args | get -o script | default "" | lines)
    for l in ($ls | first $SCRIPT_LINES) {
        print $"($INDENT)(ansi default_dimmed)(clip $LINE_CHARS $l)(ansi reset)"
    }
    if ($ls | length) > $SCRIPT_LINES {
        print $"(ansi default_dimmed)($INDENT)…  ($ls | length) lines total(ansi reset)"
    }
}

def render-call [b] {
    let name = ($b | get -o name | default "?")
    let args = ($b | get -o arguments)
    let path = { rel ($args | get -o path | default "?") }
    match $name {
        "read" => { print (head-line "→" $name $"(do $path)(read-range $args)") }
        "glob" => {
            # The search root is worth naming only when it is not the one glob
            # would have defaulted to — an "in <cwd>" on every call is a wide
            # column of nothing.
            let root = ($args | get -o path)
            let cwd = ($env.PI_SESSION_CWD? | default $env.PWD)
            let where = (if ($root == null) or ($root == $cwd) { "" } else { $"  in (rel $root)" })
            print (head-line "→" $name $"($args | get -o pattern | default "?")($where)")
        }
        "write" => {
            let bytes = ($args | get -o content | default "" | str length)
            print (head-line "→" $name $"(do $path)  \((human-bytes $bytes)\)")
        }
        "edit" => { print (head-line "→" $name (edit-summary $args)) }
        "todo" => { print (head-line "→" $name (todo-summary $args)) }
        "nu" => {
            let timeout = ($args | get -o timeout)
            let rest = (if ($timeout != null) and ($timeout != 120) { $"timeout ($timeout)s" } else { "" })
            print (head-line "→" $name $rest)
            print-script $args
        }
        _ => { print (head-line "→" $name (arg-summary $args)) }
    }
}

# --- tool results ----------------------------------------------------------

# The exit code in nu's footer line, or null when the line is not one. Matching
# the whole shape — prefix, closing paren, non-empty code — rather than slicing
# at fixed offsets keeps a truncated or foreign line a non-match.
def exit-code-of [line: string]: nothing -> any {
    let n = ($EXIT_PREFIX | str length)
    if (($line | str length) > $n) and ($line | str starts-with $EXIT_PREFIX) and ($line | str ends-with ")") {
        let code = ($line | str substring $n..<(($line | str length) - 1))
        if $code == "" { null } else { $code }
    } else {
        null
    }
}

# Lift that footer into the result's summary so the preview shows output rather
# than bookkeeping. It is only ever nu's *last* line, which is the assumption
# here: a script whose own output ended with exactly this shape would have it
# eaten — cheaper than printing the status twice on every nu call.
def split-exit [text: string]: nothing -> record {
    let ls = ($text | lines)
    if ($ls | is-empty) {
        {summary: "done", body: $text}
    } else {
        let code = (exit-code-of ($ls | last))
        if $code == null {
            {summary: "done", body: $text}
        } else {
            {summary: $"exit ($code)", body: ($ls | drop 1 | str join "\n")}
        }
    }
}

# The body a preview is taken from: nu's minus its exit-status footer, glob's
# with the cwd stripped off every path — a glob result is a column of paths,
# and absolute ones push the interesting tail off the right of the screen.
def body-of [name: string, text: string]: nothing -> string {
    match $name {
        "nu" => { (split-exit $text).body }
        "glob" => { $text | lines | each {|l| rel $l } | str join "\n" }
        _ => { $text }
    }
}

# What a result is worth saying in one line, given the call it answers. Derived
# from the call and the shape of the output, never scraped out of the tool's
# prose — except for write/edit, whose whole output *is* one such line already.
def result-summary [name: string, args: any, text: string]: nothing -> record {
    let n = ($text | lines | length)
    match $name {
        "read" => {
            # Without the call (hop renders a stream, so a result whose call it
            # never saw is possible) there is no path to name; the count alone
            # is still true.
            let path = ($args | get -o path)
            if $path == null {
                {summary: $"($n) lines", limit: $PREVIEW_LINES}
            } else {
                {summary: $"(rel $path)  ($n) lines", limit: $PREVIEW_LINES}
            }
        }
        "glob" => { {summary: $"($n) paths", limit: $PREVIEW_LINES} }
        "write" | "edit" => { {summary: (one-line $text), limit: 0} }
        "nu" => { {summary: (split-exit $text).summary, limit: $NU_PREVIEW_LINES} }
        _ => { {summary: $"($n) lines", limit: $PREVIEW_LINES} }
    }
}

# A todo result carries the full checklist snapshot in `details` (op's
# todo.ml, matching pi's todo.ts). Only `list` renders it: models add items
# one call at a time, and reprinting the growing checklist after every add is
# spam. The other actions answer with the tool's own one-line sentence
# ("Added todo #5: epsilon") — `text` is that result body, already extracted —
# the same shape write/edit results take.
def render-todo-details [d, text: string] {
    let err = ($d | get -o error)
    if $err != null {
        print (head-line "←" "todo" $"(ansi red)error: ($err)(ansi reset)")
        return
    }
    if ($d | get -o action) != "list" {
        print (head-line "←" "todo" (clip $VALUE_CHARS (one-line $text)))
        return
    }
    let todos = ($d | get -o todos | default [])
    let n = ($todos | length)
    print (head-line "←" "todo" $"($n) item\(s\)")
    if $n == 0 {
        print $"($INDENT)(ansi default_dimmed)no todos(ansi reset)"
    } else {
        for t in ($todos | first $TODO_ITEMS) {
            let mark = (if ($t | get -o done) == true {
                $"(ansi green)✓(ansi reset)"
            } else {
                $"(ansi default_dimmed)○(ansi reset)"
            })
            print $"($INDENT)($mark) (ansi magenta)#($t | get -o id | default 0)(ansi reset) ($t | get -o text | default "")"
        }
        if $n > $TODO_ITEMS {
            print $"(ansi default_dimmed)($INDENT)…  ($n - $TODO_ITEMS) more(ansi reset)"
        }
    }
}

def render-tool-result [m, calls: record] {
    let name = ($m | get -o toolName | default "?")
    let text = (block-text ($m | get -o content))
    let args = ($calls | get -o ($m | get -o toolCallId | default ""))
    let details = ($m | get -o details)
    if ($m | get -o isError) == true {
        # A failed nu call still has an exit code worth seeing beside the tag.
        let tag = (if $name == "nu" {
            $"(ansi red)[error](ansi reset) ((split-exit $text).summary)"
        } else {
            $"(ansi red)[error](ansi reset)"
        })
        print (head-line "←" $name $tag)
        # An error body is the one thing worth reading in full.
        print-body (body-of $name $text) $ERROR_PREVIEW_LINES
    } else if ($name == "todo") and ($details != null) {
        render-todo-details $details $text
    } else {
        let s = (result-summary $name $args $text)
        print (head-line "←" $name $s.summary)
        if $s.limit > 0 { print-body (body-of $name $text) $s.limit }
    }
}

# --- messages --------------------------------------------------------------

def render-thinking [b] {
    let ls = ($b | get -o thinking | default "" | lines)
    for l in ($ls | first $THINKING_LINES) {
        print $"  (ansi default_dimmed)(clip $LINE_CHARS $l)(ansi reset)"
    }
    if ($ls | length) > $THINKING_LINES {
        print $"(ansi default_dimmed)  …  ($ls | length) lines of thinking(ansi reset)"
    }
}

def render-user [m] {
    print ""
    print $"(ansi cyan_bold)### USER(ansi reset)"
    print (block-text ($m | get -o content))
}

def render-assistant [m] {
    let out = ($m | get -o usage | get -o output | default 0)
    let model = ($m | get -o model | default "?")
    print ""
    print $"(ansi blue_bold)### ASSISTANT(ansi reset) (ansi blue)[($model)]  out=($out) tok(ansi reset)"
    let content = ($m | get -o content)
    let blocks = (if (is-list $content) { $content } else { [] })
    for b in $blocks {
        if ($b | get -o type) == "thinking" { render-thinking $b }
    }
    let txt = (block-text $content)
    if ($txt | str trim | is-not-empty) { print $txt }
    for b in $blocks {
        if ($b | get -o type) == "toolCall" { render-call $b }
    }
}

def render-message [e, calls: record] {
    let m = ($e | get -o message)
    match ($m | get -o role) {
        "user" => { render-user $m }
        "assistant" => { render-assistant $m }
        "toolResult" => { render-tool-result $m $calls }
        _ => {}
    }
}

# --- other entries ---------------------------------------------------------

def render-session [e] {
    print $"(ansi green_bold)══ session ($e | get -o id | default "?") ══(ansi reset)"
    print $"(ansi green)cwd: (ansi reset) ($e | get -o cwd | default "?")"
    print $"(ansi green)time:(ansi reset) ($e | get -o timestamp | default "?")"
}

def render-model-change [e] {
    print $"(ansi yellow)[model → ($e | get -o provider | default "?")/($e | get -o modelId | default "?")](ansi reset)"
}

# --- call memory -----------------------------------------------------------

# A toolResult names its call by id and nothing else, so naming the file a read
# result came from means remembering the call — render.ml keeps a hashtable for
# exactly this. Here the map is passed in: `main` builds it in one pass over the
# file, and hop threads it through its stream (see hop.nu's send-op), since a
# closure cannot carry mutable state between entries.

# The {id: arguments} pairs an entry contributes. Empty for everything that is
# not an assistant turn with tool calls.
export def calls-of [e: record]: nothing -> record {
    let m = ($e | get -o message)
    if (($e | get -o type) != "message") or (($m | get -o role) != "assistant") {
        return {}
    }
    let content = ($m | get -o content)
    if not (is-list $content) { return {} }
    $content
    | where {|b| ($b | get -o type) == "toolCall"}
    | reduce --fold {} {|b, acc| $acc | upsert ($b | get -o id | default "?") ($b | get -o arguments) }
}

# --- entry point -----------------------------------------------------------

# Render one parsed entry record. Exported because the same dispatch serves
# both a finished file (`main`, below) and a live `op --stream` (hop reads the
# entries off stdout as they are appended) — one renderer, two arrival times.
# Unknown entry types print nothing, so a newer op streaming an entry this
# renderer predates is silently tolerated rather than fatal.
#
# `calls` is the {id: arguments} map from `calls-of`; omitting it costs only the
# path on a read result's summary line.
export def render-entry [e: record, calls: record = {}] {
    match ($e | get -o type) {
        "session" => { render-session $e }
        "model_change" => { render-model-change $e }
        "message" => { render-message $e $calls }
        _ => {}
    }
}

export def main [file: path] {
    let entries = (
        open --raw $file
        | lines
        | where {|l| ($l | str trim | is-not-empty)}
        | each {|l| try { $l | from json } catch { null } }
        | compact
    )
    # The session header's cwd is the root paths are shown relative to; a gate
    # log or a headerless fragment falls back to the working directory.
    let header = ($entries | where {|e| ($e | get -o type) == "session"} | get -o 0)
    $env.PI_SESSION_CWD = (if $header == null { $env.PWD } else { $header | get -o cwd | default $env.PWD })
    let calls = ($entries | reduce --fold {} {|e, acc| $acc | merge (calls-of $e) })
    for e in $entries { render-entry $e $calls }
}
