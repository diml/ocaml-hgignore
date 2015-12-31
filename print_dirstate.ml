let () = Dirstate.iter ~hg_root:"." ~f:(Printf.printf "%c %s\n")
