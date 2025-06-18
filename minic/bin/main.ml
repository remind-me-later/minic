open Parser
open Angstrom

let parse_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  match parse_string ~consume:Consume.All parse_program content with
  | Ok ast -> Printf.printf "Parsed successfully: %s\n" (Ast.show_program ast)
  | Error msg -> Printf.eprintf "Parse error: %s\n" msg

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  else parse_file Sys.argv.(1)
