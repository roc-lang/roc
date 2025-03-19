open Sedlexing
open Parser

exception SyntaxError of string

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\n' | "\r\n"]
let nat = [%sedlex.regexp? Plus '0' .. '9']
let string_literal = [%sedlex.regexp? '"', Star (Compl '"'), '"']

let lower =
  [%sedlex.regexp?
    'a' .. 'z', Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'')]

let upper =
  [%sedlex.regexp?
    'A' .. 'Z', Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'')]

let kernelfn = [%sedlex.regexp? "~", Star ('a' .. 'z' | '_')]

let pos_info_of_position ({ pos_lnum; pos_bol; pos_cnum; _ } : Lexing.position)
    =
  (pos_lnum, pos_cnum - pos_bol + 1)

let make (lexbuf : Sedlexing.lexbuf) tok =
  let start, fin = Sedlexing.lexing_positions lexbuf in
  let start, fin = (pos_info_of_position start, pos_info_of_position fin) in
  tok (start, fin)

let rec read (lexbuf : Sedlexing.lexbuf) =
  match%sedlex lexbuf with
  | whitespace -> read lexbuf
  | newline -> read lexbuf
  | "let" -> make lexbuf (fun i -> LET i)
  | "sig" -> make lexbuf (fun i -> SIG i)
  | "run" -> make lexbuf (fun i -> RUN i)
  | "when" -> make lexbuf (fun i -> WHEN i)
  | "is" -> make lexbuf (fun i -> IS i)
  | "end" -> make lexbuf (fun i -> END i)
  | "Str" -> make lexbuf (fun i -> STR i)
  | "Int" -> make lexbuf (fun i -> INT i)
  | "Erased" -> make lexbuf (fun i -> ERASED i)
  | "in" -> make lexbuf (fun i -> IN i)
  | "=" -> make lexbuf (fun i -> EQ i)
  | ":" -> make lexbuf (fun i -> COLON i)
  | ";" -> make lexbuf (fun i -> SEMI i)
  | "," -> make lexbuf (fun i -> COMMA i)
  | "->" -> make lexbuf (fun i -> ARROW i)
  | "(" -> make lexbuf (fun i -> LPAREN i)
  | ")" -> make lexbuf (fun i -> RPAREN i)
  | "[" -> make lexbuf (fun i -> LBRACKET i)
  | "]" -> make lexbuf (fun i -> RBRACKET i)
  | "{" -> make lexbuf (fun i -> LBRACE i)
  | "}" -> make lexbuf (fun i -> RBRACE i)
  | "*" -> make lexbuf (fun i -> STAR i)
  | "|" -> make lexbuf (fun i -> PIPE i)
  | "\\" -> make lexbuf (fun i -> LAMBDA i)
  | "." -> make lexbuf (fun i -> DOT i)
  | string_literal ->
      make lexbuf (fun i ->
          let s = Utf8.lexeme lexbuf in
          let s = String.sub s 1 (String.length s - 2) in
          let s = Scanf.unescaped s in
          STRING (i, s))
  | kernelfn ->
      make lexbuf (fun i ->
          let s = Utf8.lexeme lexbuf in
          let s = String.sub s 1 (String.length s - 1) in
          KERNELFN (i, s))
  | lower -> make lexbuf (fun i -> LOWER (i, Utf8.lexeme lexbuf))
  | upper -> make lexbuf (fun i -> UPPER (i, Utf8.lexeme lexbuf))
  | nat ->
      make lexbuf (fun i -> NUMBER (i, int_of_string @@ Utf8.lexeme lexbuf))
  | "+" -> make lexbuf (fun i -> PLUS i)
  | "-" -> make lexbuf (fun i -> MINUS i)
  | "#" -> comment lexbuf
  | eof -> EOF
  | _ ->
      raise
      @@ SyntaxError
           (Printf.sprintf "Unexpected char or sequence: %S"
              (Sedlexing.next lexbuf |> Option.get |> Uchar.to_char
             |> String.make 1))

and comment lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | newline -> read lexbuf
  | any -> comment lexbuf
  | _ -> failwith ""

let from_string s =
  let lexbuf = Utf8.from_string s in
  Sedlexing.set_position lexbuf
    { pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  lexbuf

let provider lexbuf () =
  let tok = read lexbuf in
  let start, fin = Sedlexing.lexing_positions lexbuf in
  (tok, start, fin)

let position lexbuf = Sedlexing.lexing_positions lexbuf |> fst
