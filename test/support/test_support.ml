(* Shared helpers for the op test suite. *)

let contains haystack needle =
  let n = String.length haystack and m = String.length needle in
  let rec loop i =
    if i + m > n then false
    else if String.sub haystack i m = needle then true
    else loop (i + 1)
  in
  m = 0 || loop 0

let write_file path content =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc content)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter (fun e -> rm_rf (Filename.concat path e)) (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let with_temp_dir f =
  let base = Filename.temp_file "op_test_" "" in
  Sys.remove base;
  Sys.mkdir base 0o755;
  Fun.protect ~finally:(fun () -> rm_rf base) (fun () -> f base)
