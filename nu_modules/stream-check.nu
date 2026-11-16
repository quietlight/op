# stream-check.nu — executable acceptance checks for `op --stream`:
# session-entry ndjson on stdout, final line a `{"type":"result",…}` envelope.
#
# This check costs one cheap LLM call; pass --model to pick a specific
# provider/model instead of the configured default.

# One check row.
def row [name: string, ok: bool, note: string = ""] {
    { check: $name, ok: $ok, note: $note }
}

# Run op with [args]; parse stdout as jsonl. Lines that fail to parse are
# kept as {raw: line} so the "all lines parse" check can count them.
def run-op [args: list<string>] {
    let out = (^op ...$args | complete)
    let entries = ($out.stdout | lines
        | where {|l| ($l | str trim | is-not-empty)}
        | each {|l| try { $l | from json } catch { { raw: $l } } })
    { exit: $out.exit_code, entries: $entries }
}

# The message records among stream entries.
def messages [entries] {
    $entries
    | where {|e| ($e | get -o type) == "message"}
    | each {|e| $e | get -o message | default {} }
}

def check-stream [model: any] {
    mut args = ["--stream"]
    if $model != null { $args = ($args ++ ["--model" $model]) }
    let prompt = "Reply with the single word ok and do not call any tools."
    let r = (run-op ($args ++ [$prompt]))
    let es = $r.entries
    let bad = ($es | where {|e| ($e | get -o raw) != null} | length)
    let last = ($es | reverse | get -o 0 | default {})
    [
        (row "stream: stdout has lines" (($es | length) > 0) $"got ($es | length)")
        (row "stream: every line parses as json" ($bad == 0) $"($bad) unparseable")
        (row "stream: first line is the session header"
            (($es | get -o 0 | default {} | get -o type) == "session"))
        (row "stream: an assistant entry was streamed"
            ((messages $es | where {|m| ($m | get -o role) == "assistant"} | length) > 0))
        (row "stream: last line is a typed result" (($last | get -o type) == "result"))
        (row "stream: result carries sessionId" (($last | get -o sessionId) != null))
        (row "stream: result is not an error" (($last | get -o isError) == false)
            ($last | get -o error | default ""))
        (row "stream: exit code 0" ($r.exit == 0) $"exit ($r.exit)")
    ]
}

export def main [--model (-m): string] {
    check-stream $model
}
