/*
 * MicroC Parser specification
 */

%{
  (* Auxiliary definitions *)
  open Ast

  (* Infix operator for creating a located node *)
  let (|@|) node loc = { node = node; loc = Location.to_code_position loc }

  (** Auxiliary function for getting the type-descriptor of a variable *)
  let type_of t v = 
    let (typeDesc, id) = v in 
    let f_aux init = function 
    | TypA(_,n) -> TypA(init,n)
    | TypP(_) -> TypP(init)
    | _ -> init
    in (List.fold_left f_aux t typeDesc, id)

%}

/* Tokens declarations */
%token INT CHAR BOOL VOID                             /* Types */
%token <string>ID                                     /* Identifiers */
%token <int>INT_VALUE                                 /* Carrying tokens */
%token <char>CHAR_VALUE 
%token <bool>BOOL_VALUE 
%token IF ELSE FOR WHILE RETURN NULL DO               /* keywords */
%token LROUND "(" RROUND ")"                          /* round brackets */
%token LSQUARE "[" RSQUARE "]"                        /* square brackets */
%token LCURLY "{" RCURLY "}"                          /* curly brackets */
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%"   /* arithmetic operators */
%token INCR "++" DECR "--" TIMESEQ "*=" MODEQ "%="    /* Preincrement and abbreviation */
%token PLUSEQ "+=" MINUSEQ "-=" DIVEQ "/="
%token LESS "<" LEQ "<=" GREATER ">" GEQ ">="         
%token NEQ "!=" EQ "==" 
%token AND "&&" OR "||" NOT "!"                       /* logic operators */
%token SEMICOLON ";" ASSIGN "=" ADDRESS "&" COMMA "," /* other symbols */
%token EOF

/* Precedence and associativity specification  */
%right    "="             /* lowest precedence */
%left     "||"
%left     "&&"
%left     "=="  "!="
%nonassoc ">"  "<"  ">="  "<="
%left     "+"  "-" "+=" "-="
%left     "*"  "/"  "%" "*=" "/=" "%="
%nonassoc "!"  "&"
%nonassoc "["             /* highest precedence */

/* resolve the shift/reduce conflict imposing a lower priority to the dummy token THEN */
%nonassoc THEN
%nonassoc ELSE

/* Starting symbol: the parser returns an Ast.program value */
%start program

/* Type of syntactical categories */
%type <Ast.program> program
%type <Ast.topdecl> topdecl
%type <Ast.typ * Ast.identifier> vardecl
%type <Ast.typ> typ
%type <Ast.typ list * Ast.identifier> vardesc
%type <Ast.fun_decl> fundecl
%type <Ast.stmt> block
%type <Ast.stmtordec> stmt_or_dec
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.access> lexpr
%type <Ast.expr> rexpr
%type <Ast.expr> aexpr

%%

/* Grammar specification */
program:  (* A program is a list of function or global variable declarations  *)
  | l = list(topdecl) EOF       { Prog(l) }

topdecl:  (* list of function or global variable declarations *)
  | f = fundecl                 { Fundecl(f) |@| $loc  }
  | v = varlist ";"             { Vardec(v) |@| $loc }

typ:    (* Simple types *)
  | INT                         { TypI }
  | CHAR                        { TypC }
  | BOOL                        { TypB }
  | VOID                        { TypV }

vardecl:  (* variable declaration: retrieve type information from v and merge it with t *)
  | t = typ v = vardesc         { type_of t v }

(** Variable description: a pair (type, name), where type is a conatenation of types: 
 *  empty list for simple variables;
 *  pointer to void for pointers to the following type;
 *  array of voids for arrays of elements of the following type; 
 *)
vardesc:
  | id = ID                     { ([],id) }
  | "(" v = vardesc ")"         { v }
  | "*" v = vardesc             { (TypP(TypV)::(fst v) , snd v) }
  | v = vardesc "[" i = option(INT_VALUE) "]"
                                { (TypA(TypV,i)::(fst v), snd v)}

(** Multiple declaration *)
varlist:
  | t = typ l = separated_nonempty_list(",", varinit)
                                {
                                  let vardescs, exps = List.split l in
                                  (List.map (type_of t) vardescs |> 
                                  List.combine) exps |>
                                  List.map ( (* handle unsized array *)
                                    fun ((t,i),e) -> match t,e with
                                    | TypA(_,None),[] -> ((t,i),e)
                                    | TypA(tt,None), l -> ((TypA(tt, Some(List.length l)),i),l)
                                    (* Clang don't care if the two length match, *)
                                    | TypA(_,_), _ -> ((t,i),e) (* it just emits a warning *)
                                    (* also for scalars, emits a warning and take *)
                                    | tt, x::_ -> ((tt,i),[x]) (* the first element *)
                                    | _, _ -> ((t,i),e)
                                  )
                                }

(** A declaration may have or not have an initial value *)
varinit:
  | v = vardesc                 { (v,[]) }
  | v = vardesc "=" e = initexpr
                                { (v, e) }

(** An initial value can be a value or a list of values for array *)
initexpr:
  | e = expr                    { [e] }
  | "{" l = separated_list(",", expr) "}"
                                { l }

fundecl:(* function declaration: type name ( params ) { ... } *)
  | t = typ id = ID "(" par = separated_list(",", vardecl) ")" b = block
                                { { typ = t; fname = id; formals = par; body = b } }

block:  (* a block is a list of statements or *local* variable declarations *)
  | "{" l = list(stmt_or_dec) "}"
                                { Block(l) |@| $loc  }

stmt_or_dec: (* statement or *local* variable declaration *)
  | s = stmt                    { Stmt(s) |@| $loc  }
  | v = varlist ";"             { Dec(v) |@| $loc }

stmt: (* if-then, if-then-else, while, for, return, simple expression and block of exp *)
  | RETURN e = option(expr) ";" { Return(e) |@| $loc  }
  | b = block                   { b }
  | e = option(expr) ";"        { (match e with |Some x -> Expr(x) |None -> Block([])) |@| $loc  }
  | WHILE "(" e = expr ")" s = stmt
                                { While(e, s) |@| $loc  }
  | DO s = stmt WHILE "(" e = expr ")" ";"
                                { Block([Stmt s |@| $loc; Stmt(While(e,s) |@| $loc) |@| $loc ] ) |@| $loc }
  | FOR "(" e1 = option(expr) ";" e2 = option(expr) ";" e3 = option(expr) ")" s = stmt
                                { 
                                  let first = match e1 with
                                  |Some a -> Stmt(Expr(a) |@| $loc ) |@| $loc
                                  |None   -> Stmt(Block([]) |@| $loc)|@| $loc
                                  in let third = match e3 with
                                  |Some c -> Stmt(Expr(c) |@| $loc ) |@| $loc 
                                  |None   -> Stmt(Block([]) |@| $loc)|@| $loc
                                  in let body = Block( [ (Stmt s |@| $loc); third; ] ) |@| $loc
                                  in let second = match e2 with
                                  |Some b -> Stmt(While(b,body) |@| $loc ) |@| $loc
                                  |None   -> Stmt(While((BLiteral true) |@| $loc,body) |@| $loc ) |@| $loc 
                                  in Block([ first; second; ]) |@| $loc
                                }
  | IF "(" e = expr ")" s = stmt                      %prec THEN
                                { If(e,s,Block([]) |@| $loc ) |@| $loc }
  | IF "(" e = expr ")" s1 = stmt ELSE s2 = stmt      
                                { If(e,s1,s2) |@| $loc }

expr: (* expression: -> expr *)
  | e = rexpr                   { e }
  | e = lexpr                   { Access(e) |@| $loc }

lexpr: (* lhs expression: -> access *)
  | id = ID                     { AccVar(id) |@| $loc }
  | "(" e = lexpr ")"           { e }
  | "*" e = lexpr               { AccDeref(Access(e) |@| $loc ) |@| $loc }
  | "*" e = aexpr               { AccDeref(e) |@| $loc }
  | l = lexpr "[" e = expr "]"  { AccIndex(l, e) |@| $loc }

rexpr: (* rhs expression: -> expr *)
  | e = aexpr                   { e }
  | id = ID "(" l = separated_list(",", expr) ")"
                                { Call(id, l) |@| $loc }
  | l = lexpr "=" e = expr      { Assign(l,e) |@| $loc }
  | "!" e = expr                { UnaryOp(Not,e) |@| $loc }
  | "-" e = expr                { UnaryOp(Neg,e) |@| $loc }
  | e1 = expr op = binop e2 = expr
                                { BinaryOp(op,e1, e2) |@| $loc }

  | e1 = lexpr op = shortbinop e2 = expr
                                { BinaryOpEq(op,e1,e2) |@| $loc }
  | "++" e = lexpr              { BinaryOpEq(Add, e, ILiteral 1 |@| $loc) |@| $loc }
  | "--" e = lexpr              { BinaryOpEq(Sub, e, ILiteral 1 |@| $loc) |@| $loc }
  | e = lexpr "++"              { Postincr(e) |@| $loc }
  | e = lexpr "--"              { Postdecr(e) |@| $loc }

aexpr: (* access exp -> expr *)
  | i = INT_VALUE               { ILiteral(i) |@| $loc }
  | c = CHAR_VALUE              { CLiteral(c) |@| $loc }
  | b = BOOL_VALUE              { BLiteral(b) |@| $loc }
  | NULL                        { Access(AccVar("NULL") |@| $loc) |@| $loc }
  | "(" e = rexpr ")"           { e }
  | "&" e = lexpr               { Addr(e) |@| $loc }

%inline binop: (* Binary operations *)
  | "+"                         { Add }
  | "-"                         { Sub }
  | "*"                         { Mult }
  | "%"                         { Mod }
  | "/"                         { Div }
  | "&&"                        { And }
  | "||"                        { Or }
  | "<"                         { Less }
  | ">"                         { Greater }
  | "<="                        { Leq }
  | ">="                        { Geq }
  | "=="                        { Equal }
  | "!="                        { Neq }

%inline shortbinop:
  | "+="                        { Add }
  | "-="                        { Sub }
  | "*="                        { Mult }
  | "/="                        { Div }
  | "%="                        { Mod }
;