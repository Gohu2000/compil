open Format
open Lexing
open Parser

let usage = "usage: koka [options] file.koka"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".koka") then
      raise (Arg.Bad "no .koka extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let f = Parser.file Lexer.next_token lb in
  List.iter (fun (id, fb) -> printf "%s" id) f;
  close_in c;
  exit 0
  (*if !parse_only then exit 0;
  Interp.file f*)
