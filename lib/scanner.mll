{
    open Parser

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    (* Keyword table *)
    let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

	  let keyword_table =
		create_hashtable 11 [
			("if", 	IF);
			("else",ELSE);
      ("for", FOR);
      ("while",WHILE);
      ("do", DO);
      ("return",RETURN);
      ("NULL",NULL);
	  	("int", INT);
      ("char",CHAR);
      ("bool",BOOL);
      ("void",VOID);
		]

    (* shorthand for Location.to_lexeme_position *)
    let position l = Location.to_lexeme_position l
}

(** Definition section *)
let digit   = ['0'-'9']
let hex     = ("0x"|"0X") ['0'-'9' 'A'-'F']*
let int     = digit+ | hex
let bool 	  = ("true"|"false")
let char    = "'" [^ '\\' '\''] "'"
let escchar = "'\\"
let id 		  = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white   = [' ' '\t']

(* Scanner specification *)

rule next_token = parse
  | int as num		    { try INT_VALUE(int_of_string num) 
                        with Failure _ -> raise (Lexing_error(position lexbuf, 
                        "Integer literal is too large to be represented.")) 
                      }
	| bool as b					{ BOOL_VALUE (bool_of_string b)}
	| char as c					{ CHAR_VALUE (c.[1]) }
  | escchar           { escape lexbuf }
	| id as word        {
												try Hashtbl.find keyword_table word
												with Not_found -> ID word
											}
  | ','               { COMMA }
  | ';'               { SEMICOLON }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '%'               { MOD }
  | "=="              { EQ }
  | "!="              { NEQ }
  | '='               { ASSIGN }
  | '<'               { LESS }
  | '>'               { GREATER }
  | "<="              { LEQ }
  | ">="              { GEQ }
  | '('               { LROUND }
  | ')'               { RROUND }
  | '['               { LSQUARE }
  | ']'               { RSQUARE }
  | '{'               { LCURLY }
  | '}'               { RCURLY }
  | "&&"              { AND }
  | "||"              { OR }
  | '!'               { NOT }
  | '&'               { ADDRESS }
  | "++"              { INCR }
  | "--"              { DECR }
  | "+="              { PLUSEQ }
  | "-="              { MINUSEQ }
  | "*="              { TIMESEQ }
  | "/="              { DIVEQ }
  | "%="              { MODEQ }
	| "/*"							{ comments lexbuf }
  | "//" [^ '\n']*    (* eat up one-line comments *)
  | white             (* eat up whitespace *)
											{ next_token lexbuf }
  | '\n'              { Lexing.new_line lexbuf; next_token lexbuf}
  | eof               {EOF}
  | _                 { raise (Lexing_error(position lexbuf, 
                        "Unexpected character "^(Lexing.lexeme lexbuf)))
                      }

(* In C are not allowed nested multi-line comments *)
and comments = parse
	| "*/"  		        { next_token lexbuf }
	| '\n'     	        { Lexing.new_line lexbuf; comments lexbuf }
  | _					        { comments lexbuf }
  | eof               { raise (Lexing_error(position lexbuf, "Unterminated comment")) }

(* sequence of escape characters: https://en.wikipedia.org/wiki/Escape_sequences_in_C *)
and escape = parse
  | "n\'"             { CHAR_VALUE ('\n') }
  | "r\'"             { CHAR_VALUE ('\r') }
  | "t\'"             { CHAR_VALUE ('\t') }
  | "v\'"             { CHAR_VALUE ('\011') }
  | "0\'"             { CHAR_VALUE ('\000') }
  | "\'\'"            { CHAR_VALUE ('\'') }
  | "\\\'"            { CHAR_VALUE ('\\') }
  | "\"\'"            { CHAR_VALUE ('\"') }
  | "?\'"             { CHAR_VALUE ('\063') }
  | "a\'"             { CHAR_VALUE ('\007') }
  | "b\'"             { CHAR_VALUE ('\b') }
  | "e\'"             { CHAR_VALUE ('\027') }
  | "f\'"             { CHAR_VALUE ('\012') }
  | "/\'"             { CHAR_VALUE ('/') }
  | _                 { raise (Lexing_error(position lexbuf, 
                        "Illegal backslash escape in character "^(Lexing.lexeme lexbuf)))
                      }