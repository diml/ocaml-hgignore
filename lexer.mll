{
type syntax = Glob | Regexp

let glob_any = Re.any

let mk regexps = Re.compile (Re.seq regexps)
}

let space = [' ' '\t']
let eol = ('\n' | eof)

rule file acc simple syntax = parse
| eof
  { acc, simple }
| '#' [^'\n']* eol
  { Lexing.new_line lexbuf; file acc simple syntax lexbuf }
| space* eol
  { Lexing.new_line lexbuf; file acc simple syntax lexbuf }
| "syntax:" space* "regexp" space* eol
  { Lexing.new_line lexbuf; file acc simple Regexp lexbuf }
| "syntax:" space* "glob" space* eol
  { Lexing.new_line lexbuf; file acc simple Glob lexbuf }
| "syntax:"
  { failwith "syntax error" }
| "glob:" ([^'\n' '*' '[' ']' '{' '}']+ as s) eol
  { file acc (s :: simple) syntax lexbuf }
| "glob:"
  { file (glob [] lexbuf :: acc) simple syntax lexbuf }
| "regexp:"
  { file (regexp  lexbuf :: acc) simple syntax lexbuf }
| ""
  { match syntax with
    | Glob   -> file (glob [] lexbuf :: acc) simple syntax lexbuf
    | Regexp -> file (regexp  lexbuf :: acc) simple syntax lexbuf
  }

and glob acc = parse
| eol
    { Lexing.new_line lexbuf;
      Re.seq (Re.rep Re.any :: List.rev (Re.eos :: acc))
    }
| [^'\n' '*' '[' ']' '{' '}']+ as s
    { glob (Re.str s :: acc) lexbuf }
| "**"
    { glob (glob_any :: acc) lexbuf }
| "*"
    { glob (glob_any :: acc) lexbuf }
| '[' ([^'\n' ']']* as s) ']'
    { glob (Re.set s :: acc) lexbuf }
| '{'
    { let opts = Re.alt (glob_braces lexbuf) in
      glob (opts :: acc) lexbuf }
| ""
  { failwith "syntax error at %s:%i" }

and glob_braces = parse
 | [^'\n' '}' ',']* as s ','
   { Re.str s :: glob_braces lexbuf }
 | [^'\n' '}' ',']* as s '}'
   { [Re.str s] }
 | ""
  { failwith "syntax error" }

and regexp = parse
| ([^'\n']* as s) eol
   { Lexing.new_line lexbuf; Re.Pcre.re s }

{
  let load fn =
    let ic = open_in fn in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- {
      pos_fname = fn;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
    let l, simple = file [] [] Glob lexbuf in
(*    let l =
      Re.seq [ Re.rep Re.any
             ; Re.alt simple
             ; Re.eos
             ]
      :: l
      in*)
    close_in ic;
    (Re.compile (Re.alt l), simple)
}
