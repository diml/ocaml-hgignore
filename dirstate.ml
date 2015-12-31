let iter ~hg_root ~f =
  let ic = open_in (Filename.concat hg_root ".hg/dirstate") in
  seek_in ic 40;
  try
    while true do
      let state = input_char ic in
      seek_in ic (pos_in ic + 12);
      let len = input_binary_int ic in
      let s = really_input_string ic len in
      let fn =
        if String.contains s '\000' then
          String.sub s 0 (String.index s '\000')
        else
          s
      in
      f state fn
    done
  with End_of_file ->
    close_in ic
