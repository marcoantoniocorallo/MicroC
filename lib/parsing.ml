exception Syntax_error of Location.lexeme_pos * string

let parse (scanner : Lexing.lexbuf -> Parser.token) (lexbuf : Lexing.lexbuf) : Ast.program = 
  try Parser.program scanner lexbuf
  with
  |Scanner.Lexing_error(p,s) -> raise(Scanner.Lexing_error(p,s))
  |Parser.Error -> raise(Syntax_error(Location.to_lexeme_position lexbuf,"Syntax error.\n"))
  |exn -> raise(Syntax_error(Location.to_lexeme_position lexbuf, Printexc.to_string exn))
