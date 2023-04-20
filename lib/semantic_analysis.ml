open Ast
open Symbol_table

exception Semantic_error of Location.code_pos * string

(*********************** Type Definitions ***********************)

(** Overrides the Ast types including a special type Null; *)
type nullable_typ = 
  | TypI 
  | TypB 
  | TypC 
  | TypV 
  | TypA of nullable_typ * int option
  | TypP of nullable_typ
  | Null

(** Symbol table informations:
 *  sort : tells if the symbol is a variable or a function
 *  ty : var type if variable, return type if function
 *  params : type of the parameters if function. Empty list if variable.
 *)
type info = {
  sort  : sort;
  ty    : nullable_typ;
  params: nullable_typ list;
} and sort = Fun | Var

(*********************** Utilities ***********************)

(** Auxiliary function for making info record *)
let make_info sort ty params = {sort = sort; ty = ty; params = params;}

(** Check if a given type is boolean *)
let is_boolean : nullable_typ -> bool = function | TypB -> true | _ -> false

(** Check if a given type is an array *)
let is_array : nullable_typ -> bool = function | TypA(_,_) -> true |_ -> false

(** Check if two given types are equal *)
let match_types t1 t2 = 
  let rec f t1 t2 = match t1,t2 with 
  | Null, Null | Null, TypP _ | TypP _, Null -> true
  | TypI, TypI | TypB, TypB | TypC, TypC | TypV, TypV -> true
  | (TypA (typeA1,_)),(TypA (typeA2,_)) -> f typeA1 typeA2
  | TypP typeP1, TypP typeP2 -> f typeP1 typeP2
  | _,_ -> false
in f t1 t2

(** Auxiliary function for raising a semantic error *)
let raise_semantic loc s = raise(Semantic_error(loc,s))

(** Auxiliary function for displaying different debugging informations depending on the case *)
let raise_duplicate ?(context="") ide loc = match ide with
  |"print" 
  |"getint" -> raise_semantic loc ("Error: "^ide^" is a reserved keyword for a built-in function.")
  |_ when context="formal" -> raise_semantic loc ("Error: duplicate of formal "^ide)
  |_ when context="fun"-> raise_semantic loc ("Error: redefinition of function "^ide^".")
  |_ -> raise_semantic loc ("Error: redefinition of identifier "^ide^".")

(** Auxiliary function for pretty printing types in debugging informations;
 *  the ppx-derived function shows them in a non-really-pretty format (Ast.TypB...);
 *)
let show_typ = 
  let rec f = function 
  | Null -> "NULL"
  | TypB -> "bool"
  | TypI -> "int"
  | TypV -> "void"
  | TypC -> "char"
  | TypP t -> "pointer to "^(f t)
  | TypA (t,_) -> "array of "^(f t)  
  in f

let show_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

(** Unary operator for converting a typ to the corresponding nullable-typ *)
let rec (!@) : typ -> nullable_typ = function
  | TypI -> TypI
  | TypB -> TypB
  | TypC -> TypC
  | TypV -> TypV
  | TypA (a,n) -> TypA(!@a, n)
  | TypP p -> TypP !@p

(** Auxiliary function for getting a subsequence of a list of elements;
 *  Returns l[0:k], where k is the index of the first item such that predicate k = true
 *)
let sublist l predicate = 
  let rec f_aux acc = function
  | [] -> acc |> List.rev
  | x::_ when predicate x -> x::acc |> List.rev
  | x::xs -> f_aux (x::acc) xs
  in f_aux [] l

(** Auxiliary function: try to unify with a Return node *)
let is_a_return = function 
  | {node=Stmt({node=Return _; _}); _} -> true 
  |_ -> false

(** An easy way for printing some warning, like in the case of integer overflow. *)
let emit_warning s (loc : Location.code_pos option) = 
  let pos = 
    if Option.is_none loc then "" else let lloc = Option.get loc in " at position "^
    (string_of_int lloc.Location.start_line)^":"^(string_of_int lloc.Location.start_column)^" - "^
    (string_of_int lloc.Location.end_line)^":"^(string_of_int lloc.Location.end_column) in
  Printf.eprintf "%s\n" ("Warning: "^s^pos)

(*********************** Dead Code Elimination ***********************)

(** Dead code eliminator: scan the AST pruning instructions after return instructions;
 *  the analysis returns a reduced AST, that is a sound pruning of the original one;
 *  that is, it doesn't remove an instruction if it could be reached;
 *  it only prune instructions that certainly won't be reached by the control-flow.
 *  The analysis does "backpropagation". I.e.:
 *  A program like
 *                  i1;
 *                  :
 *                  im{
 *                    :
 *                    ij = return;
 *                    :
 *                    ik;
 *                  }
 *                  :
 *                  in;
 *
 *  is converted into
 *
 *                  i1;
 *                  :
 *                  im{
 *                    :
 *                    ij = return;
 *                  }
 *  It also recognizes literal condition for if/while statement and prune unreachable stmts;
 *)
let rec deadcode_elimination (Prog decls) = 
  let reduced_prog, reduced = 
    List.map deadcode_topdecl decls |> 
    List.split |> 
    fun (x,y) -> (x, List.fold_left (||) false y) in
  if reduced then emit_warning "Dead-Code has been found and eliminated" None;
  Prog(reduced_prog)

(** Deadcode-eliminator for top-declarations: 
 *  uses a sub-routine for functions, the identity for vardeclarations;
 *)
and deadcode_topdecl = function
  | {node=Fundecl f;loc} -> 
    let new_f, reduced = deadcode_fun f in 
    {node=Fundecl (new_f); loc}, reduced
  | x -> x,false

(** Deadcode-eliminator for functions:
 *  maps each fun f(){ i1;...;in } into a fun f(){ i1;...;im }, with m<=n 
 *)
and deadcode_fun { typ = t; fname = f; formals = p; body = b } = 
  match b with
  | {node=Block l; loc} -> 
    let new_block, reduced = deadcode_block l in 
    { typ = t; fname = f; formals = p; body = {node=Block(new_block); loc}}, reduced
  | _ -> raise_semantic Location.dummy_code_pos "Error! Function without body!"

(** Deadcode-eliminator for blocks:
 *  maps each block of instructions { i1;...;in } into a block { i1;...;im }, with m<=n;
 *  for statements, check if contain dead instructions and prune it!
 *)
and deadcode_block l =
  (sublist l is_a_return |> 
  List.map (
    function 
    | {node=Stmt s; loc} -> 
      let ds,truth = deadcode_stmt s in 
      (match ds with 
      | Some x -> (Some {node=Stmt x; loc}, truth)
      | None -> (None, truth) )
    | e -> (Some e, false) ) |> 
  sublist) snd |>
  List.split |>
  fun (x,y) -> (List.filter_map (fun i->i) x, List.exists (fun x->x) y)

(** Deadcode-eliminator for statements:
 *  applies deadcode elimination to the statements that have a block;
 *)
and deadcode_stmt stmt = match stmt with
  | {node=If(e, s1, s2); loc} -> 
    let (dds1,t1), (dds2,t2) = deadcode_stmt s1, deadcode_stmt s2 in 
    let ds1, ds2 = (Option.value dds1 ~default:{node=Block([]);loc=s1.loc}),(Option.value dds2 ~default:{node=Block([]);loc=s2.loc}) in
    (match is_true e, is_empty ds1, is_empty ds2 with
    | true, true, _ -> (None, false)
    | true, false, _ -> (Some ds1, t1)
    | _, _, _ -> 
    match is_false e, is_empty ds1, is_empty ds2 with
    | true, _, true -> (None, false)
    | true, _, false -> (Some ds2, t2)
    (* undecidable condition's truth *)
    | _, true, true -> (Some {node=Expr({node=e.node;loc=e.loc}); loc=e.loc}, false)
    | _, true, false -> (Some {node=If({node=UnaryOp(Not,({node=e.node;loc=e.loc}));loc=e.loc}, ds2, {node=Block([]);loc=s1.loc}); loc}, false)
    | _, false, true -> (Some {node=If(e, ds1, {node=Block([]);loc=s2.loc}); loc}, false) 
    | _, false, false -> (Some {node=(If(e,ds1,ds2)); loc=loc}, t1 && t2)
    )
  | {node=While(e,s); loc} ->
    let dds, t = deadcode_stmt s in 
    let ds = Option.value dds ~default:{node=Block([]);loc=s.loc} in
    (match is_true e, is_empty ds with
    | true, true -> (None, false)
    | true, false -> (Some {node=While(e,ds); loc}, t)
    | _, _ ->
    match is_false e, is_empty ds with 
    | true, _ -> (None, false)
    (* undecidable condition's truth *)
    | _, true 
    | _, false -> ((Some {node=While(e,ds); loc}, false))
    )
  | {node=Expr e; loc} -> (Some {node=Expr e; loc}, false)
  | {node=Return e; loc} -> (Some {node=Return e; loc}, true)
  | {node=Block b; loc} -> 
    let db, truth = deadcode_block b in match db with 
    | [] -> (None, false)
    | _ -> (Some {node=Block db; loc}, truth)

(** Auxiliary function for checking if a statement is empty *)
and is_empty s = match s.node with
  | Block b -> 
    List.map (fun x->x.node) b |> 
    List.for_all (function | Stmt s -> is_empty s | _ -> false)
  | If(e, s1, _) when is_true e && is_empty s1 -> true
  | If(e, _, s2) when is_false e && is_empty s2 -> true
  | While (e, _) when is_false e -> true
  | _ -> false 

and is_true e = match e.node with | BLiteral t -> t | _ -> false
and is_false e = match e.node with | BLiteral t -> not t | _ -> false

(*********************** Type checking ***********************)

(** A program is a list of top declaration: 
 *  checks and adds each top-level declaration to the symbol table,
 *  then checks the body of the functions;
 *  Finally, checks that there's the definition of the main function.
 *)
let rec check_prog (Prog decls) table = 
  List.iter (fun x -> check_topdecl x table) decls;
  List.iter (fun x -> match (x.node) with Fundecl f -> check_fun f table (x.loc) |_ -> ()) decls;
  if check_main table then Prog decls
  else raise_semantic Location.dummy_code_pos 
  "Error: the file must provide a definition of the main function."

(** Type checker for top declaration: 
 *  checks function signatures and variable declarations, 
 *  and adds them to the global scope
 *)
and check_topdecl e table = match e.node with
  | Fundecl f -> check_signatures table f (e.loc)
  | Vardec l -> check_varlist l table e.loc; check_constant l e.loc 

(** Type checker for initialized declaration: for each declaration
 *  check the declaration itself, then check the expression assigned to it;
 *  If the exp's type matches the declarated type, return; 
 *)
and check_varlist l table loc = 
  List.iter (
    fun ((t,i),e) -> (check_vardec !@t i table loc;
    match e with 
    | [] -> () 
    | [x] -> (* simple variable -> check if types match *)
      let tx = type_of_expr x.node table x.loc in 
      if match_types !@t tx then () 
      else raise_semantic loc ("Type Error: type "^(show_typ !@t)^" doesn't match type "^(show_typ tx))
    | list -> (* array: check if type match with all elements *)
      match !@t with
      | TypA(tt, Some n) -> 
        let m = List.length list in 
        if m>n then emit_warning "excess elements in array initializer" (Some loc); 
        if List.for_all (fun y -> type_of_expr y.node table y.loc |> match_types tt) list then ()
        else raise_semantic loc ("Type Error: the element doesn't match type "^(show_typ !@t))
      | _ -> emit_warning "excess elements in scalar initializer" (Some loc) (* should be unreachable *)
    ) ) l

(** Global variable can be initialized only with constant values! *)
and check_constant l loc = 
  let ll = l |> List.split |> snd in
  if List.for_all (
    function 
    | [] -> true
    | xs -> 
      let rec f_aux e = match e.node with
      | ILiteral _
      | CLiteral _
      | BLiteral _ -> true 
      | UnaryOp(_,e) -> f_aux e 
      | BinaryOp(_,e1,e2) -> f_aux e1 && f_aux e2
      | Access({node=AccVar("NULL"); loc=_}) -> true (* representation of null values *)
      |_ -> false in
      List.for_all f_aux xs
  ) ll then () 
  else raise_semantic loc ("Error: initializer element is not a compile-time constant")

(** Type checker for function signature:
 *  checks the return type and the parameter types;
 *  then adds the function to the global scope in the symbol table;
 *)
and check_signatures table { typ = t; fname = name; formals = params; body = _ } loc =
  match t with (* Check return type *)
  | TypI | TypB | TypC | TypV -> 
    (try (* Check param types and adds the signature to the symbol table *)
      List.iter (fun (t,v) -> check_param_type !@t v loc) params;
      add_entry name (make_info Fun !@t (List.map (fun (x,_) -> !@x) params)) table |> ignore
    with 
    | DuplicateEntry ide -> raise_duplicate ide loc ~context:"fun"
    | Failure s -> raise_semantic loc s)
  | t -> raise_semantic loc ("Bad return type for "^name^": "^(show_typ !@t))

(** Check if a function parameter is well-typed; that is:
 *  - is not void;
 *  - array elements are not void;
 *  - array are 1-dimensional;
 *  - sized-array must have a size of at least 1;
 *)
and check_param_type t v loc = match t, v with
  | TypV, _ -> raise_semantic loc ("Type Error: attempt to use or declare a void variable!")
  | TypA (TypV,_),_ -> raise_semantic loc ("Type Error: attempt to use or declare a void array!")
  | TypA (TypA(_,_),_ ),_ -> raise_semantic loc ("Type Error: multi-dimensional array are not supported!")
  | TypA (_,Some n), _ when n<=0 -> raise_semantic loc ("Error: array should have size of at least 1 element")
  | _,_ -> ()

(** Type checker for variable declaration: checks if the variable is well-typed,
 *  then adds an entry <var, info> into the current block of the symbol table *)
and check_vardec t v table loc = 
  check_var_type t v loc; 
  try add_entry v (make_info Var t []) table |> ignore
  with
  | DuplicateEntry ide -> raise_duplicate ide loc
  | Failure s -> raise_semantic loc s

(** Check if a declaration is well-typed;
 *  Same checks as for function parameters, but in addition arrays must have a defined size *)
and check_var_type t v loc = match t, v with
  | TypA (_,None),_ -> raise_semantic loc "Error: array should have size of at least 1 element"
  | _,_ -> check_param_type t v loc

(** Type checker for functions:
 *  Creates a new scope into which adds the arguments of the function as variables;
 *  Checks the body of the function and then removes again the scope.
 *)
and check_fun { typ = t; fname = _; formals = params; body = body } table loc =
  let new_scope = begin_block table in 
  try (* adds the parameters into the scope like local variables *)
    List.fold_left (fun tab (t,i) -> add_entry i (make_info Var !@t []) tab) new_scope params |> ignore;
    match body.node with 
    |Block b -> (* type checking of the body *)
      List.iter (fun x -> check_stmtordec (x.node) !@t new_scope (x.loc)) b; 
      end_block new_scope |> ignore
    | _ -> failwith "Error! function with non-block body - parser failed!!!"
  with
  | DuplicateEntry ide -> raise_duplicate ide loc ~context:"formal"
  | Failure s -> raise_semantic loc s

(** Type checker for statements: unpacks the stmt and invokes the sub-type-checker *)
and check_stmt stmt ret table loc = match stmt with
  | If (e, s1, s2) -> check_if e s1 s2 ret table loc
  | While (e, s) -> check_while e s ret table 
  | Expr e -> check_expr (e.node) table (e.loc)
  | Return e -> check_return e ret table loc
  | Block b -> check_block b ret table

(** Type checker for If-then-else: checks that the guard is a boolean expression;
 *  then invokes the type checker for the then-else statements *)
and check_if e s1 s2 ret table loc = 
  if is_boolean (type_of_expr (e.node) table (e.loc))
  then (check_stmt (s1.node) ret table (s1.loc); check_stmt (s2.node) ret table (s2.loc))
  else raise_semantic loc "Type Error: Boolean expression was expected"

(** Type checker for expression: check the expression and returns its type;
 *  for complex expressions, delegates the recognition to dedicated routines.
 *  Carry out an optional parameter that tells if the expression is arithmetically negated
 *  in order to correctly checks an eventual integer underflow
 *)
and type_of_expr ?(negation_flag=false) exp table loc = match exp with
  | CLiteral _ -> TypC
  | BLiteral _ -> TypB
  | ILiteral n -> check_int n loc negation_flag; TypI
  | Access acc -> type_of_access (acc.node) table (acc.loc)
  | Assign (acc, e) -> type_of_assign acc e table loc
  | Addr acc -> type_of_addr acc table
  | UnaryOp (op, e) -> type_of_unaryop op e table loc
  | BinaryOp (op, e1, e2) -> type_of_binaryop op e1 e2 table loc
  | Call (f, params) -> type_of_call f params table loc

  | BinaryOpEq(op, e1, e2) -> 
    (match op, type_of_access e1.node table e1.loc, type_of_expr e2.node table e2.loc with
    | (Add | Sub | Mult | Div | Mod), TypI, TypI -> TypI
    | _, t1, t2 -> raise_semantic loc ("Type Error: operator undefined for types "^(show_typ t1)^", "^(show_typ t2)))
  | Postincr(x)
  | Postdecr(x) -> 
    (match type_of_access x.node table x.loc with
    | TypI -> TypI
    | t -> raise_semantic loc ("Type Error: incremental operators expect an integer value. Have "^(show_typ t)))

(** Checks if an integer literal causes an overflow/underflow 
 *  an underflow is something like Neg(2147483649), 
 *  so the function has a parameter that tells if the number is positive or negative
 *)
and check_int n loc neg = match n,neg with
  | _,false when n >= 2147483648 -> emit_warning "Possible Integer Overflow occurs" (Some loc)
  | _,true when n >= 2147483649 -> emit_warning "Possible Integer Underflow occurs" (Some loc)
  | _,_ -> ()

(** Type checker for while: checks that the guard is a boolean expression;
 *  then invokes the type checker for the body statement *)
and check_while guard c ret table = 
  if is_boolean (type_of_expr (guard.node) table (guard.loc))
  then check_stmt (c.node) ret table (c.loc)
  else raise_semantic (guard.loc) "Type Error: Boolean expression was expected"

(** Checks the expression in a given symbol table *)
and check_expr e table loc = (type_of_expr e table loc) |> ignore

(** Checks if the signature return type and the actual return type match; 
 *  raise semantic error otherwise *)
and check_return e ret table loc = 
  let t_e = (match e with Some t -> type_of_expr (t.node) table (t.loc) |_ -> TypV) in 
  if t_e=ret then ()
  else raise_semantic loc ("Type Error: return type was expected "^(show_typ ret)^" but have "^(show_typ t_e))

(** Type checker for blocks: a block is a list of either statements or  declarations;
 *  scan the list invoking the corresponding type-checker routines in a new scope *)
and check_block b ret table = 
  let new_scope = begin_block table in 
  List.iter (fun x -> check_stmtordec (x.node) ret new_scope (x.loc)) b;
  end_block new_scope |> ignore
and check_stmtordec e ret table loc = match e with
  (*| Dec (t, var) -> check_vardec !@t var table loc *)
  | Dec l -> check_varlist l table loc
  | Stmt s -> check_stmt (s.node) ret table (s.loc)

(** Recognizes the type of an access expression. 
 *  Raise a semantic error if:
 *  - Try to access to a function value;
 *  - Try to use an undeclared identifier;
 *  - Try to dereference a non-pointer variable;
 *  - Try to access to an element of an array using a non-integer value;
 *  - Try to access to an array but the variable is not an array;
 *)
and type_of_access acc table loc = match acc with
  | AccVar x -> 
    (try 
      let info = lookup x table in 
      match info.sort with 
      | Var -> info.ty
      | Fun -> raise_semantic loc ("Type Error: "^x^" is a function!")
    with Failure _ -> raise_semantic loc ("Error: use of undeclared identifier \'"^x^"\'"))
  | AccDeref x -> (match (type_of_expr (x.node) table (x.loc)) with
    | TypP t -> t
    | Null ->  raise_semantic loc ("Type Error: attempt to dereference a null-value.")
    | _ as t -> raise_semantic loc ("Type Error: indirection requires pointer operand (\'"^(show_typ t)^"\' invalid)"))
  | AccIndex (arr, i) -> 
    let t_arr = (type_of_access (arr.node) table (arr.loc)) in 
    let t_i = (type_of_expr (i.node) table (i.loc)) in 
    match t_arr, t_i with
    | (TypA (t,_)),TypI -> t
    | (TypA (_,_)), _ -> raise_semantic loc "Type Error: array subscript is not an integer"
    | Null, _ -> raise_semantic loc "Type Error: attempt to access to a null-value"
    | _, _ -> raise_semantic loc ("Type Error: subscripted value is not an array - "^(show_typ t_arr))

(** Recognizes the type of an assignment, 
 *  that is the type of the variable if it matches the type of the value assigned to it
 *  Raise a semantic error if the type of the variable doesn't match the type of the value
 *)
and type_of_assign acc e table loc = 
  let type_of_acc = type_of_access (acc.node) table (acc.loc) in 
  let type_of_exp = type_of_expr (e.node) table (e.loc) in 
  if  (match_types type_of_acc type_of_exp) then 
    if not (is_array type_of_acc) then type_of_acc
    else raise_semantic loc "Type Error: attempt to assign a value to an array!"
  else raise_semantic loc ("Type Error: attempt to assign a value of type "^(show_typ type_of_exp)^" to a variable of type "^(show_typ type_of_acc))

(** Recognizer for address operations: recognizes the type of the argument and returns a pointer to it *)
and type_of_addr acc table = TypP (type_of_access (acc.node) table (acc.loc))

(** Type checker for unary operator application:
 *  check that the operand is an integer or a boolean and raise a semantic error otherwise 
 *)
and type_of_unaryop op e table loc = 
  match op, (type_of_expr (e.node) table (e.loc) ~negation_flag:true) with
  | Neg, TypI -> TypI
  | Not, TypB -> TypB
  | _,_ -> raise_semantic loc "Type Error: negation of wrong-typed value!"

(** Type checker for binary operations: 
 *  checks that the operation is defined on the arguments types and returns the type of the result 
 *  arithmetic ops are defined only on integers;
 *  comparison ops are defined on integers and characters;
 *  equality ops are defined on integers, characters, boolean and pointers;
 *  logic ops are defined only on boolean values;
 *  raise a semantic error for other combination of operators and types;
 *)
and type_of_binaryop op e1 e2 table loc = 
  let t_e1 = type_of_expr (e1.node) table (e1.loc) in 
  let t_e2 = type_of_expr (e2.node) table (e2.loc) in 
  match op, t_e1, t_e2 with
  | (Add | Sub | Mult | Div | Mod), TypI, TypI -> TypI 
  | (Equal | Neq | Less | Leq | Greater | Geq), TypI, TypI -> TypB 
  | (Equal | Neq | Less | Leq | Greater | Geq), TypC, TypC -> TypB 
  | (Equal | Neq | And | Or ), TypB, TypB -> TypB
  | (Equal | Neq ), TypP p1, TypP p2 when match_types p1 p2 -> TypB 
  | _, _, _ -> raise_semantic loc
  ("Type Error: the operator "^(show_binop op)^" is not defined on types "^(show_typ t_e1)^" and "^(show_typ t_e2))

(** Type checker of a function call: raise semantic error if 
 *  - the identifier invoked is a variable;
 *  - the identifier invoked is not found;
 *  - the passed arguments do not match the formal parameters list;
 *)
and type_of_call f params table loc = 
  try
    let info = lookup f table in
    let t_params = List.map (fun x -> type_of_expr (x.node) table (x.loc)) params in 
    match info.sort with
    | Fun -> 
      begin
      try 
        if   List.for_all2 match_types t_params (info.params) then info.ty
        else raise_semantic loc ("Type Error: the arguments passed to function "^f^
                                " don't match the expected types.\nExpected ("^
                                (List.map show_typ info.params |> String.concat ",")^"), have ("^
                                (List.map show_typ t_params |> String.concat ",")^")" )
      with Invalid_argument _ -> 
        raise_semantic loc ("Wrong number of arguments to function call: "^f^
        " expects "^(string_of_int(List.length (info.params)))^", have "^(string_of_int (List.length params)))
      end
    | Var -> raise_semantic loc ("Type Error: "^f^" is a variable, not a function!")
  with Failure _ -> raise_semantic loc ("Error: use of undeclared identifier \'"^f^"\'")

(** Checks if the file provides a definition for the main function, that can be
 *  - int main()
 *  - void main()
 *)
and check_main table = 
  try 
    match lookup "main" table with
    | {sort=Fun; ty=(TypI|TypV); params=[]} -> true
    | _ -> false
  with Failure _ -> false

(** Type checker - provides a global scope, with built-in functions print and getint *)
let type_check program = 
  empty_table |>
  begin_block |>  
  add_entry "NULL" (make_info Var Null []) |>
  add_entry "print" (make_info Fun TypV [TypI]) |>
  add_entry "getint" (make_info Fun TypI []) |>
  check_prog (deadcode_elimination program)