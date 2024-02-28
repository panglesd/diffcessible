let () =
  let version =
    Scanf.sscanf Sys.ocaml_version "%d.%d" (fun major minor -> (major, minor))
  in
  let basename =
    if version < (4, 08) then "lwd_infix_compat" else "lwd_infix_letop"
  in
  let file =
    match Sys.argv.(1) with
    | "intf" -> basename ^ ".mli"
    | "impl" -> basename ^ ".ml"
    | _ -> assert false
  in
  let ic = open_in_bin file in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  print_string content
