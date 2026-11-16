The text and JSON forms of `--list messages` intentionally expose different
amounts of tree detail: text is compact for humans, JSON includes parentId for
programmatic callers.

  $ export HOME=$PWD
  $ umask 077  # keep the config private; op warns on world-readable models.json
  $ SESSION_DIR=".op_global/sessions/--$(pwd | sed 's#^/##; s#[/:\\]#-#g')--"
  $ mkdir -p "$SESSION_DIR"
  $ cat > "$SESSION_DIR/20260630T000000_s1.jsonl" <<'EOF'
  > {"type":"session","id":"s1","timestamp":"2026-06-30T00:00:00.000Z","cwd":"/tmp","version":3}
  > {"type":"message","id":"u1","parentId":null,"timestamp":"2026-06-30T00:00:01.000Z","message":{"role":"user","content":[{"type":"text","text":"root prompt"}],"timestamp":1782777601000}}
  > {"type":"message","id":"a1","parentId":"u1","timestamp":"2026-06-30T00:00:02.000Z","message":{"role":"assistant","content":[{"type":"text","text":"first answer"}],"timestamp":1782777602000}}
  > {"type":"message","id":"b1","parentId":"u1","timestamp":"2026-06-30T00:00:03.000Z","message":{"role":"assistant","content":[{"type":"text","text":"branch answer"}],"timestamp":1782777603000}}
  > EOF

Plain text lists marker, entry id, role, and preview. It does not include the
parent id column.

  $ op --list messages --sessionId s1
  *  u1       user       root prompt
     a1       assistant  first answer
     b1       assistant  branch answer

JSON includes parentId and isFork for callers that need the tree structure.

  $ op --list messages --sessionId s1 --json
  {"kind":"messages","sessionId":"s1","messages":[{"id":"u1","parentId":null,"role":"user","preview":"root prompt","isFork":true},{"id":"a1","parentId":"u1","role":"assistant","preview":"first answer","isFork":false},{"id":"b1","parentId":"u1","role":"assistant","preview":"branch answer","isFork":false}]}

The sessions text table includes the message count column before the summary.

  $ op --list sessions
  s1                                    06-30 12:00     3  root prompt

Summer timestamps use the displayed date's local offset, not the epoch offset.

  $ cat > "$SESSION_DIR/20260701T000000_s2.jsonl" <<'EOF'
  > {"type":"session","id":"s2","timestamp":"2026-01-01T00:00:00.000Z","cwd":"/tmp","version":3}
  > {"type":"message","id":"u2","parentId":null,"timestamp":"2026-01-01T00:00:01.000Z","message":{"role":"user","content":[{"type":"text","text":"summer prompt"}],"timestamp":1767225601000}}
  > EOF
  $ op --list sessions --limit 1
  s2                                    01-01 13:00     1  summer prompt
  … 1 more; use --limit N to show more
