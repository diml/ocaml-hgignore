let rec find_root dir subdir =
  let fn = Filename.concat dir ".hg" in
  if Sys.file_exists fn then
    (dir, subdir)
  else
    let parent = Filename.dirname dir in
    if parent = dir then
      failwith "hg root not found"
    else
      let base = Filename.basename dir in
      find_root parent
        (match subdir with
         | None -> Some base
         | Some s -> Some (Filename.concat base s))
;;

let rec load_hgignore dir =
  let fn = Filename.concat dir ".hgignore" in
  if Sys.file_exists fn then
    Lexer.load fn
  else
    let parent = Filename.dirname dir in
    if parent = dir then
      failwith ".hgignore not found"
    else
      load_hgignore parent
;;

module StringSet = Set.Make(String)

let is_prefix =
  let rec loop s prefix i stop =
    i = stop || s.[i] = prefix.[i] && loop s prefix (i + 1) stop
  in
  fun s ~prefix ->
    let len = String.length prefix in
    String.length s >= len && loop s prefix 0 len

let is_suffix =
  let rec loop s suffix i j =
    j < 0 || s.[i] = suffix.[j] && loop s suffix (i - 1) (j - 1)
  in
  fun s ~suffix ->
    let len_s = String.length s and len_suffix = String.length suffix in
    len_s >= len_suffix && loop s suffix (len_s - 1) (len_suffix - 1)

let bsearch =
  let rec loop t key a b =
    if a = b then
      None
    else
      let c = (a + b) / 2 in
      let d = String.compare t.(c) key in
      if d < 0 then
        loop t key (c + 1) b
      else if d > 0 then
        loop t key a c
      else
        Some c
  in
  fun t key -> loop t key 0 (Array.length t)

let () =
  let cwd = Sys.getcwd () in
  let root, subdir = find_root cwd None in
  let prefix =
    match subdir with
    | None -> ""
    | Some s -> s ^ "/"
  in
  (* First, filter out files that are tracked by hg. From a clean repo this will often
     discard everything. *)
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  (* We make this assumption later *)
  Array.iter (fun fn -> assert (not (String.contains fn '/'))) files;
  Array.sort String.compare files;
  let keep = Array.make (Array.length Sys.argv) true in
  Dirstate.iter ~hg_root:root ~f:(fun state fn ->
    if is_prefix fn ~prefix then
      let suffix =
        let n = String.length prefix in
        String.sub fn n (String.length fn - n)
      in
      match bsearch files suffix with
      | None -> ()
      | Some i -> keep.(i) <- false);
  let files =
    let rec loop i acc =
      if i = Array.length files then
        acc
      else
      if keep.(i) then
        loop (i + 1) (files.(i) :: acc)
      else
        loop (i + 1) acc
    in
    loop 0 []
  in
  match files with
  | [] -> ()
  | _ ->
    let re, simple_globs = Lexer.load (Filename.concat root ".hgignore") in
    (* Technically, we can build this regexp and be done with it:

       {[
         let re = Re.(alt [ re; seq [ rep any; alt (List.map str simple_globs); eos ] ])
       ]}

       But we have a lot of simple globs (of the form "glob:glob_without_special_things")
       and using simple heuristics makes things a lot faster.
    *)
    let matched, others = List.partition (fun fn -> Re.execp re (prefix ^ fn)) files in
    let more_matched =
      match others with
      | [] -> []
      | files ->
        (* Second, reduce the number of simple globs. This makes things faster. *)
        let full_path_globs, basename_globs =
          List.partition (fun s -> String.contains s '/') simple_globs
        in
        let exact_basenames =
          match subdir with
          | None -> StringSet.empty
          | Some subdir ->
            List.fold_left
              (fun acc glob ->
                 let can_match = is_suffix subdir ~suffix:(Filename.dirname glob) in
                 if can_match then
                   StringSet.add (Filename.basename glob) acc
                 else
                   acc)
              StringSet.empty full_path_globs
        in
        let matched, others =
          List.partition (fun fn -> StringSet.mem fn exact_basenames) files
        in
        let more_matched =
          match others, basename_globs with
          | [], _ | _, [] -> []
          | files, _ ->
            let re =
              Re.(compile (seq [rep any; alt (List.map Re.str basename_globs); eos]))
            in
            List.filter (fun fn -> Re.execp re fn) files
        in
        List.rev_append more_matched matched
    in
    let matched = List.rev_append more_matched matched in
    List.iter (Printf.printf "%s\n") matched
