(* 
                         CS 51 Final Project
                      MiniML -- Lexical Analyzer

 *)

{
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 8 [
                       ("if", IF);
                       ("in", IN);
                       ("then", THEN);
                       ("else", ELSE);
                       ("let", LET);
                       ("raise", RAISE);
                       ("rec", REC);
                       ("true", TRUE);
                       ("false", FALSE);
                       ("lambda", FUNCTION);
                       ("fun", FUNCTION);
                       ("function", FUNCTION)
                     ]
                     
  let sym_table = 
    create_hashtable 8 [
                       ("=", EQUALS);
                       ("<", LESSTHAN);
                       ("^", CONCAT);
                       (".", DOT);
                       ("->", DOT);
                       (";;", EOF);
                       ("~-", NEG);
                       ("+", PLUS);
                       ("-", MINUS);
                       ("*", TIMES);
                       ("(", OPEN);
                       (")", CLOSE)
                     ]
}

let digit = ['0'-'9']
let partial = digit* '.' digit*
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let sym = ['(' ')'] | (['+' '-' '*' '.' '=' '~' ';' '<' '"' '^' '>']+)
let char = ['a' - 'z'] | ['A' - 'Z'] 
let space = [' ']
let str = '"' (char* | digit* | sym* | space*) '"'


rule token = parse
  | digit+ as inum
        { let num = int_of_string inum in
          INT num
        }
  | partial+ as inum
        { let num = float_of_string inum in
          FLOAT num 
        }
  | id as word
        { try
            let token = Hashtbl.find keyword_table word in
            token 
          with Not_found ->
            ID word
        }
  | str+ as word
        { let word = String.sub word 1 ((String.length word) - 2) in 
          STRING word
        }
  | sym as symbol
        { try
            let token = Hashtbl.find sym_table symbol in
            token
          with Not_found ->
            printf "Ignoring unrecognized token: %s\n" symbol;
            token lexbuf
        }
  | '{' [^ '\n']* '}'   { token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']     { token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn and skip unrecognized characters *)
        { printf "Ignoring unrecognized character: %c\n" c;
          token lexbuf
        }
  | eof
        { raise End_of_file }
