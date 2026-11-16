# Windows compatibility

*Re-evaluated 2026-09-12 against the codebase, the installed Eio 1.5 sources
and upstream eio `main`. This replaces a much longer costing; the decision it
recorded is summarised here so it is not rediscovered.*

## Verdict

`op` does not run on native Windows and is not being ported. WSL is the
supported route for Windows users; the README's "A POSIX host" line stands.

The port is deliberately kept cheap rather than done. Everything that is pure
string handling and testable on Linux is in place, so what remains is one
isolated function plus the toolbox.

## Done (platform-neutral hygiene, tested on Linux)

- `Gate.normalize_segments` maps `\` to `/` and strips a drive prefix before
  the dot-file check, on every host. Without this a Windows path would reach
  the gate with its backslashes unsplit, and the deny could silently
  under-match. Fixtures in `test/test_gate.ml`.
- `Util.home_dir` (`HOME`, then `USERPROFILE`, then `HOMEDRIVE`+`HOMEPATH`)
  replaces five direct `HOME` reads (config, sessions, gate log, module-dir
  `~/` expansion, plugins).
- `Util.path_list_sep` (`;` on win32, `:` elsewhere) threads through `which`,
  nu's `--include-path` join and the `pathMode: host` child PATH.
- `setup_nu` catches the `Failure` that `eio_windows` raises from
  `process_mgr` and drops the `nu` tool with a warning, the same path as "nu
  not on PATH". A native `op.exe` therefore runs everything except `nu` rather
  than crashing: chat, `read`/`write`/`edit`/`glob`/`todo`, sessions, handoff
  and the machine views.

## What a real port would still need

1. **Process spawning.** `eio_windows` implements `net`, `fs`, `clock` and the
   rest of stdenv, but `process_mgr` is `failwith "... not supported on
   Windows yet"` (upstream `main`, 2026-09-12; not on the issue #125
   checklist, so treat it as unscheduled). `op` touches it in exactly one
   function, `Nu.spawn_capture`. A stdlib replacement on
   `Unix.create_process_env` / `waitpid [WNOHANG]` / `kill sigkill` is
   feasible but needs a reader thread, because `Unix.select` is sockets-only
   on Windows. It would be longer than the Eio version and untested on its
   target. Keep Eio on POSIX; add the `Unix` one behind the same signature
   only if a Windows user materialises.
2. **The toolbox.** `build_toolbox` symlinks externals into a temp dir, which
   needs a privilege on Windows. Copy or hardlink instead, and make `which`
   honour `PATHEXT`. Only matters once (1) exists.
3. **The dependency chain.** `mirage-crypto`, `tls-eio` and `cohttp-eio` carry
   no `available:` constraint, but nobody has built them in a Windows switch
   for this project. Unverified, and the first thing to check before
   scheduling (1).
4. **Build tooling.** The Makefile is bash/coreutils; building *on* Windows
   stays WSL or Git Bash.

Minor, already handled or harmless: `ca-certs` 1.0.3 loads the Windows `ROOT`
store; `Session.dir_for` already maps both separators; `glob` is pure OCaml
(its `path:` argument is not normalized, a rough edge to fix with (1)).
