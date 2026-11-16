# loc.nu — line counts for the op codebase
#
# Splits OCaml sources into three buckets mirroring `make count`:
#   impl  — implementation files  (.ml)  under lib/ and bin/
#   intf  — interface files       (.mli) under lib/ and bin/
#   test  — OCaml test sources    (.ml + .mli) under test/
#
# Uses only Nushell builtins (glob, open --raw, lines) so it passes Gate_nu.
#
# Usage:
#   use loc.nu
#   loc              # summary table
#   loc --per-file   # + per-file breakdown
#   loc --json       # single JSON object

# count a list of files -> {files, lines}
def count-files [files: list<string>] {
    if ($files | is-empty) {
        {files: 0, lines: 0}
    } else {
        let per = ($files | each {|f|
            try { open --raw $f | lines | length } catch { 0 }
        })
        {files: ($files | length), lines: ($per | math sum)}
    }
}

# per-file line counts sorted descending
def per-file-rows [files: list<string>] {
    $files | each {|f|
        let n = try { open --raw $f | lines | length } catch { 0 }
        {file: $f, lines: $n}
    } | sort-by lines --reverse
}

export def main [
    --per-file  # also list every file with its line count (descending)
    --json      # emit a single JSON object instead of the table
] {
    let impl_files = ((glob "lib/**/*.ml") ++ (glob "bin/**/*.ml") | sort)
    let intf_files = ((glob "lib/**/*.mli") ++ (glob "bin/**/*.mli") | sort)
    let test_ml = (glob "test/**/*.ml" | sort)
    let test_mli = (glob "test/**/*.mli" | sort)
    let test_files = ($test_ml ++ $test_mli | sort)

    let impl = (count-files $impl_files)
    let intf = (count-files $intf_files)
    let test = (count-files $test_files)
    let total_files = $impl.files + $intf.files + $test.files
    let total_lines = $impl.lines + $intf.lines + $test.lines

    let cram_files = (glob "test/**/*.t" | sort)
    let cram = (count-files $cram_files)

    if $json {
        let payload = {
            impl: {files: $impl.files, lines: $impl.lines, globs: ["lib/**/*.ml", "bin/**/*.ml"]},
            intf: {files: $intf.files, lines: $intf.lines, globs: ["lib/**/*.mli", "bin/**/*.mli"]},
            test: {files: $test.files, lines: $test.lines, globs: ["test/**/*.ml", "test/**/*.mli"]},
            cram: {files: $cram.files, lines: $cram.lines, globs: ["test/**/*.t"]},
            total: {files: $total_files, lines: $total_lines}
        }
        return ($payload | to json)
    }

    print ((ansi green_bold) + "op line counts" + (ansi reset) + "  — generated from live glob; no _build / _opam")
    print ((ansi dark_gray) + "impl = lib+bin .ml  ·  intf = lib+bin .mli  ·  test = test .ml+.mli  ·  cram = test .t" + (ansi reset))
    print ""

    let rows = [
        {bucket: "impl (.ml)",      files: $impl.files, lines: $impl.lines, pattern: "lib/**/*.ml, bin/**/*.ml"},
        {bucket: "intf (.mli)",     files: $intf.files, lines: $intf.lines, pattern: "lib/**/*.mli, bin/**/*.mli"},
        {bucket: "test (.ml/.mli)", files: $test.files, lines: $test.lines, pattern: "test/**/*.ml, test/**/*.mli"},
        {bucket: "cram (.t)",       files: $cram.files, lines: $cram.lines, pattern: "test/**/*.t"},
        {bucket: "total (ml+mli)",  files: $total_files, lines: $total_lines, pattern: "—"},
    ]
    # `table` is display-only; pipe through `to text` so it renders inside op.
    print ($rows | table --index false | to text)

    if $per_file {
        print ""
        print ((ansi yellow_bold) + "--per-file, largest first" + (ansi reset))

        for b in [
            {label: "impl", files: $impl_files},
            {label: "intf", files: $intf_files},
            {label: "test", files: $test_files},
            {label: "cram", files: $cram_files},
        ] {
            if ($b.files | is-empty) { continue }
            print ""
            print $"(ansi cyan_bold)($b.label) — ($b.files | length) files(ansi reset)"
            print (per-file-rows $b.files | table --index false | to text)
        }
    }
}
