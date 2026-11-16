# hop-reader.nu — @path Tab completion for hop. Needs the patched nu
# (~/nushell, branch completer-input): stock nu cannot parse `input
# --completer`, which is exactly why this one call site lives outside hop.nu.
# Pair it with hop in config.nu or at the prompt — order doesn't matter, hop
# reads $env.HOP_READER each time it prompts:
#
#   use ~/ocaml/op/nu_modules/hop-reader.nu
#   use ~/ocaml/op/nu_modules/hop.nu
#
# The reader contract (see hop.nu's runtime section): a closure taking the
# history file path and the @token completer closure, returning the typed
# line with reedline semantics (ctrl-d -> null, ctrl-c -> throws).
export-env {
    $env.HOP_READER = {|history, completer|
        input --reedline --history-file $history --completer $completer "hop> "
    }
}
