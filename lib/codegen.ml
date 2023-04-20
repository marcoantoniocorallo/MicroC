open Ast
open Symbol_table

(*********************** Global Definitions ***********************)

(** Define the context and the module: 
 *  top-level container for all LLVM global data 
 *  and op-level container for all other LLVM Intermediate Representation
 *)
let context = Llvm.global_context()
let llvm_module = Llvm.create_module context "MicroC"

(** Define simple llvm types and useful constants *)
let ll_int = Llvm.i32_type context
let ll_bool = Llvm.i1_type context
let ll_char = Llvm.i8_type context
let ll_void = Llvm.void_type context
let ll_zero = Llvm.const_int ll_int 0

(** Built-in function prototypes *)
let print =
  (Llvm.function_type ll_void [| ll_int |] |> Llvm.declare_function "print") llvm_module
let getint = 
  (Llvm.function_type ll_int [||] |> Llvm.declare_function "getint") llvm_module

(*********************** Utilities ***********************)

(** Auxiliary function for mapping each microC type into a llvm type 
 *  unsized array as function arguments became pointers;
 *  pointer to unsized array became pointer to array of size 0;
 *)
let rec type_of ?(is_pointed=false) = function
  | TypI -> ll_int
  | TypB -> ll_bool
  | TypC -> ll_char
  | TypV -> ll_void
  | TypP t -> Llvm.pointer_type (type_of t ~is_pointed:true)
  | TypA (t, Some n) -> (* declaration of [n x t] *)
                        Llvm.array_type (type_of t) n 
  | TypA (t, None) ->   (* unsized array are allowed only as function argument 
                         * array are converted to pointers,
                         * pointers to array remain pointers to array *)
                        if is_pointed then Llvm.array_type (type_of t) 0
                        else Llvm.pointer_type (type_of t)

(** Auxiliary function for mapping a parameter (type, ide) to a llvm type
 *  as parameter, both sized and unsized arrays become pointers *)
and to_param_type (t,_) = match t with
  | TypA (t, _) -> Llvm.pointer_type (type_of t)
  | other -> type_of other

(** Shorthand for classifying the type of elements of an aggregate type *)
let element_type e = Llvm.type_of e |> Llvm.element_type |> Llvm.classify_type

(** Auxiliary function for getting the value of an address *)
let value_of addr ibuilder = match element_type addr with
  | Llvm.TypeKind.Array -> addr (* access to the array *)
  | _ -> Llvm.build_load addr "" ibuilder (* load value *)

(** Auxiliary function for checking if a statement is empty *)
let rec is_empty s = match s with
  | Block b -> 
    List.map (fun x->x.node) b |> 
    List.for_all (function | Stmt s -> is_empty s.node | _ -> false)
  | If(e, s1, _) when is_true e && is_empty s1.node -> true
  | If(e, _, s2) when is_false e && is_empty s2.node -> true
  | While (e, _) when is_false e -> true
  | _ -> false 

and is_true e = match e.node with | BLiteral t -> t | _ -> false
and is_false e = match e.node with | BLiteral t -> not t | _ -> false

(** Auxiliary function that tells if exists a terminator in a given bb *)
let there_is_terminator bb = 
  bb |> Llvm.block_terminator |> Option.is_some

(** An easy way for printing some warning, like in the case of integer overflow. *)
let emit_warning s (loc : Location.code_pos option) = 
  let pos = 
    if Option.is_none loc then "" else let lloc = Option.get loc in " at position "^
    (string_of_int lloc.Location.start_line)^":"^(string_of_int lloc.Location.start_column)^" - "^
    (string_of_int lloc.Location.end_line)^":"^(string_of_int lloc.Location.end_column) in
  Printf.eprintf "%s\n" ("Warning: "^s^pos)

(** Auxiliary function for initialing a global variable *)
let rec init_value x t = match x.node with
  | ILiteral i -> Llvm.const_int ll_int i
  | BLiteral b -> Llvm.const_int ll_bool (if b then 1 else 0)
  | CLiteral c -> Llvm.const_int ll_char (int_of_char c)
  | Access({node=AccVar("NULL"); loc=_}) -> Llvm.const_pointer_null t
  | UnaryOp(Not, e) -> Llvm.const_not (init_value e t)
  | UnaryOp(Neg, e) -> Llvm.const_neg (init_value e t)
  | BinaryOp(op,e1,e2) -> 
    let e' = init_value e1 t in 
    let e'' = init_value e2 t in 
    (match op with
    | Add -> Llvm.const_add e' e'' 
    | Sub -> Llvm.const_sub e' e'' 
    | Mult-> Llvm.const_mul e' e'' 
    | Div -> Llvm.const_sdiv e' e'' 
    | Mod -> Llvm.const_srem e' e'' 
    | Neq -> Llvm.const_icmp (Llvm.Icmp.Ne) e' e'' 
    | Less ->Llvm.const_icmp (Llvm.Icmp.Slt) e' e'' 
    | Leq -> Llvm.const_icmp (Llvm.Icmp.Sle) e' e'' 
    | Geq -> Llvm.const_icmp (Llvm.Icmp.Sge) e' e'' 
    | Equal -> Llvm.const_icmp (Llvm.Icmp.Eq) e' e'' 
    | Greater -> Llvm.const_icmp (Llvm.Icmp.Sgt) e' e'' 
    | Or ->  Llvm.const_and e' e''
    | And -> Llvm.const_and e' e''
    )
  | _ -> failwith(" Unreachable state! ")

(** Auxiliary function that adds padding null values into a too short list; *)
let fill_with l c size = 
  let rec f_aux l acc n = match l, n with
  | [], 0 -> List.rev acc
  | [], _ -> f_aux [] (c::acc) (n-1)
  | x::xs, _ -> f_aux xs (x::acc) (n-1)
  in f_aux l [] size

(*********************** Code Generation ***********************)

(** A program is a list of top-declarations: 
 *  iter on the declarations, adding globals and function names into the symbol table; 
 *  then iter on the functions for generating the bodies;
 *)
let rec gen_prog (Prog decls) table = 
  let l = List.map (fun x->x.node) decls in
  List.iter (fun x-> gen_topdecl x table) l;
  List.filter_map (function | Fundecl f -> Some f | _ -> None) l |>
  List.iter (fun x -> gen_fun x table)

(** Code-generator for top-declaration: 
 *  invokes the corresponding code-generator *)
and gen_topdecl e table = match e with
  | Fundecl f -> gen_signature f table
  | Vardec l -> gen_global_list l table

(** Code-generator for global variable: 
 *  generates the code for the global variable, 
 *  if the initial value is not specified use the null value; 
 *  then adds information into the symbol table *)
and gen_global_list l table = 
  List.iter (
    fun ((t,i),e) -> 
      let ty = type_of t in 
      let ll_g = Llvm.define_global i (match e with 
      | [ ] -> Llvm.const_null ty 
      | [x] -> 
        (match t with
        | TypA(_,_) -> (* A[] = {x} *)
          let ll_e = Llvm.element_type ty in 
          Llvm.const_array ll_e [| init_value x ll_e |]
        | _ -> init_value x ty)
      | list -> (* Can be only array A[n] = {x1,...,xk} note: n can be lt or gt k! *)
        let ll_e = Llvm.element_type ty in 
        Llvm.const_array ll_e (List.map (fun x -> init_value x ll_e) list |> Array.of_list)
      ) llvm_module in 
      add_entry i ll_g table |> ignore
  ) l

(** Define the function and adds the information in the symbol table; *)
and gen_signature { typ = t; fname = name; formals = params; body = _ } table =
  let t_ret = type_of t in (* return type *)
  let t_param = List.map to_param_type params in (* param types *)
  let t_f = Llvm.function_type t_ret (Array.of_list t_param) in (* function type *)
  let f = Llvm.define_function name t_f llvm_module in  (* define llvm function *)
  add_entry name f table |> ignore 

(** Code-generator for functions body:
 *  Retrieve the type of the function from the symbol table;
 *  creates new scope into which allocates parameters and generates the code for the body;
 *)
and gen_fun { typ = _; fname = name; formals = params; body = body } table = 
  let f = lookup name table in 
  let t_ret = f |> Llvm.type_of |> Llvm.element_type |> Llvm.return_type in 
  let ibuilder = Llvm.builder_at_end context (Llvm.entry_block f) in (* ibuilder *)
  Llvm.set_value_name "" (Llvm.entry_block f |> Llvm.value_of_block) |> ignore;

  (* adds function information in the global scope and creates a new scope for the body *)
  let new_scope = begin_block table in

  (* generates the code to allocate and store arguments into the stack of the function, *)
  (* then adds local variables to the local scope *)
  allocate_params (List.combine (List.map snd params) (Array.to_list (Llvm.params f))) ibuilder new_scope;
  
  (* generates the code of the function's body *)
  (match body.node with 
  | Block b -> 
    List.iter (fun x -> gen_stmtordec (x.node) f ibuilder new_scope |> ignore) b
  | _ -> failwith "Error! function with non-block body - parser failed!!!");

  (* generates the terminator of the function if it doesn't exist *)
  if not (there_is_terminator (Llvm.insertion_block ibuilder)) then
    if t_ret=ll_void then (Llvm.build_ret_void ibuilder |> ignore)
    else (Llvm.build_ret (Llvm.const_null t_ret) ibuilder |> ignore );
  end_block new_scope |> ignore

(** Allocates function parameters into the stack:
 *  builds the alloca instructions, adds the parameters into the symbol table 
 *  and builds the store instructions;
 *)
and allocate_params params ibuilder table =
  let allocate_param (ide,t) = 
    (Llvm.build_alloca t "" ibuilder |> add_entry ide) table |> ignore in 
  let store_param (ide,v) = Llvm.build_store v (lookup ide table) ibuilder |> ignore in 
  List.iter allocate_param (List.map (fun(i,t)->(i,Llvm.type_of t)) params);
  List.iter store_param params;

(** Code-generator for statements: 
 *  unpacks the argument and invokes the corresponding routine 
 *  returns a value that tells if the statements has a return instruction;
 *)
and gen_stmt stmt f ibuilder table = 
  match stmt with
  | If (e, s1, s2) -> gen_if (e.node) (s1.node) (s2.node) f ibuilder table
  | Expr e -> gen_expr (e.node) f ibuilder table |> ignore
  | While (e, s) -> gen_while (e.node) (s.node) f ibuilder table
  | Return e -> gen_return e f ibuilder table
  | Block b -> gen_block b f ibuilder table

(** Code-generator for if: check if there's some statement in the else-branch, 
 *  then invokes the routines for generating an if-then-else or just an if-then statement *)
and gen_if e s1 s2 f ibuilder table = 
  if is_empty s2 
  then gen_ifthen e s1 f ibuilder table    
  else gen_ifthenelse e s1 s2 f ibuilder table

(** Code-generator for if-then-else:
 *  creates basic blocks for then-branch, else-branch and for when the control-flow join;
 *  then fills then-bb and else-bb with instructions and an unconditional jump to join-bb;
 *  the entry basic block ends with a conditional jump;
 *)
and gen_ifthenelse e s1 s2 f ibuilder table = 
  let bb_then = Llvm.append_block context "" f in
  let bb_else = Llvm.append_block context "" f in 
  let bb_join = Llvm.append_block context "" f in

  (* generates code for the guard and for the conditional branch *)
  let ll_e = gen_expr e f ibuilder table in 
  Llvm.build_cond_br ll_e bb_then bb_else ibuilder |> ignore;

  (* generates code for then-statement *)
  Llvm.position_at_end bb_then ibuilder;
  gen_stmt s1 f ibuilder table;

  (* if not returned into the statements, create br instruction *)
  if not (there_is_terminator (Llvm.insertion_block ibuilder)) 
  then Llvm.build_br bb_join ibuilder |> ignore;

  (* generates code for else-statement *)
  Llvm.position_at_end bb_else ibuilder;
  gen_stmt s2 f ibuilder table;

  (* if not returned into the statements, create br instruction *)
  if not (there_is_terminator (Llvm.insertion_block ibuilder)) 
  then Llvm.build_br bb_join ibuilder |> ignore;

  Llvm.position_at_end bb_join ibuilder |> ignore

(** Code-generator for if-then statement:
 *  like the if-then-else one, but without the else-branch.
 *  The successors of entry basic blocks are the then bb and the join bb;
 *)
and gen_ifthen e s1 f ibuilder table = 
  let bb_then = Llvm.append_block context "" f in
  let bb_join = Llvm.append_block context "" f in

  (* generates code for the guard and for the conditional branch *)
  let ll_e = gen_expr e f ibuilder table in 
  Llvm.build_cond_br ll_e bb_then bb_join ibuilder |> ignore;

  (* generates code for then-statement *)
  Llvm.position_at_end bb_then ibuilder;
  gen_stmt s1 f ibuilder table |> ignore;

  (* if not returned into the statements, create br instruction *)
  if not (there_is_terminator (Llvm.insertion_block ibuilder))
  then Llvm.build_br bb_join ibuilder |> ignore;

  Llvm.position_at_end bb_join ibuilder |> ignore

(** Code-generator for expressions: unpacks the expression type and invokes the sub-routine *)
and gen_expr ?(address=false) ?(t_null=ll_void) e f ibuilder table = match e with
  | ILiteral n -> Llvm.const_int ll_int n
  | BLiteral b -> Llvm.const_int ll_bool (if b then 1 else 0)
  | CLiteral c -> Llvm.const_int ll_char (Char.code c) (* character ASCII value *)
  | Access a -> gen_access ~address:address ~t_null:t_null (a.node) f ibuilder table
  | Addr a -> gen_access ~address:true (a.node) f ibuilder table
  | Assign (a,e) -> gen_assign (a.node) (e.node) f ibuilder table
  | UnaryOp (op, e) -> gen_uop op (e.node) f ibuilder table
  | BinaryOp (op,e1,e2)-> gen_bop op (e1.node) (e2.node) f ibuilder table
  | BinaryOpEq (op,e1,e2)-> gen_bop_eq op (e1.node) (e2.node) f ibuilder table
  | Call (ide,params) -> gen_call ide (List.map (fun x->x.node) params) f ibuilder table
  | Postincr(x) -> gen_post x f true ibuilder table 
  | Postdecr(x) -> gen_post x f false ibuilder table 

(** Code-generator for access to an identifier; 
 *  it can generates just the load of the register, 
 *  or also the load of the value stored there;
 *  For assignments, for example, is needed the address;
 *  when dereferencing a pointer, instead, we want a load of the pointed value;
 *)
and gen_access ?(address=false) ?(t_null=ll_void) a f ibuilder table = match a with
  | AccVar "NULL" -> Llvm.const_pointer_null t_null
  | AccVar i ->
    let v = lookup i table in 
    if address then v (* symbol table maintain address of identifiers *)
    else value_of v ibuilder

  | AccDeref e -> (* type system ensures e is a pointer -> ~address:false *)
    (let ll_e = gen_expr ~address:false (e.node) f ibuilder table in
    if address then ll_e else value_of ll_e ibuilder)

  | AccIndex (a,i) -> (* generates llvm base and index for accessing to the element *)
    let ll_i = gen_expr (i.node) f ibuilder table in   
    let ll_a = gen_access (a.node) f ibuilder table in 
    let instr = (* Address of an array or array converted to pointer *)
      match ll_a |> element_type with
      | Llvm.TypeKind.Array -> (* address of an array *)
        Llvm.build_in_bounds_gep ll_a [| ll_zero; ll_i |] "" ibuilder
      | _ -> (* array converted to pointer *)
        Llvm.build_in_bounds_gep ll_a [| ll_i |] "" ibuilder
    in if address then instr else value_of instr ibuilder

(** Code-generator for assignment;
 *  invokes the code-generators for access and expressions and generates a store instruction;
 *)
and gen_assign a e f ibuilder table = 
  let ll_a = gen_access a f ~address:true ibuilder table in 
  let ll_e = gen_expr e f ~t_null:(ll_a |> Llvm.type_of |> Llvm.element_type) ibuilder table in 
  Llvm.build_store ll_e ll_a ibuilder |> ignore; ll_e 

(** Code-generator for unary operators: 
 *  for negation of integers, generates a "sub 0 e" instruction;
 *  for negation of booleans, generates a "xor e true" instruction;
 *  note: in C there are not booleans, so the negation ("!") of an expression is like:
 *        icmp ne e 0 |> xor true |> zext i1 to i32
 *  here this stuff is useless: negation is an operator from i1 to i1;
 *)
and gen_uop op e f ibuilder table = 
  let ll_e = gen_expr e f ibuilder table in 
  match op with
  | Neg -> Llvm.build_neg ll_e "" ibuilder 
  | Not -> Llvm.build_not ll_e "" ibuilder 

(** Code-generator for binary-operator instructions:
 *  note: in C there is no bool type, so a comparison between two integers is like
 *        icmp eq i32 e1 e2 |> zext i1 to i32
 *        same thing is for comparison of chars: Clang should generate something like:
 *        sext i8 e1 to i32 |> icmp eq i32 (sext i8 e2 to i32) |> zext i1 to i32
 *  here this stuff is useless: icmp is built from i1/i8/i32 values to i1;
 *)
and gen_bop op e1 e2 f ibuilder table = 
  let gen e = gen_expr e f ibuilder table in 
  (match op with
  | Add -> fun x y -> gen y |> Llvm.build_add (gen x)
  | Sub -> fun x y -> gen y |> Llvm.build_sub (gen x)
  | Mult-> fun x y -> gen y |> Llvm.build_mul (gen x)
  | Div -> fun x y -> gen y |> Llvm.build_sdiv (gen x)
  | Mod -> fun x y -> gen y |> Llvm.build_srem (gen x)
  | Neq -> fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Ne (gen x)
  | Less ->fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Slt (gen x)
  | Leq -> fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Sle (gen x)
  | Geq -> fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Sge (gen x)
  | Equal -> fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Eq (gen x)
  | Greater -> fun x y -> gen y |> Llvm.build_icmp Llvm.Icmp.Sgt (gen x)
  | Or ->  fun x y _ builder -> build_shortcircuit_or x y f builder table 
  | And -> fun x y _ builder -> build_shortcircuit_and x y f builder table 
  ) e1 e2 "" ibuilder

(** Code-generator for assignment-abbreviation operands; *)
and gen_bop_eq op a e f ibuilder table = 
  let ll_a = gen_access ~address:true a f ibuilder table in 
  let value = Llvm.build_load ll_a "" ibuilder in
  let ll_e = gen_expr e f ibuilder table in 
  let v = (match op with
  | Add -> Llvm.build_add
  | Sub -> Llvm.build_sub
  | Mult -> Llvm.build_mul
  | Div -> Llvm.build_sdiv
  | Mod -> Llvm.build_srem
  | _ -> failwith("unreachable!")) value ll_e "" ibuilder in
  Llvm.build_store v ll_a ibuilder |> ignore; v

(** Code-generator for post-increment and post-decrement:
 *  generate the assignment of the incremented value and returns the initial one;
 *)
and gen_post x f increment ibuilder table = match increment with
  | true -> (* increment *)
    let var = gen_access ~address:true x.node f ibuilder table in 
    let value = Llvm.build_load var ""  ibuilder in 
    (Llvm.build_add value (Llvm.const_int ll_int 1) "" ibuilder |> 
    Llvm.build_store) var ibuilder |> ignore; value
  | _ -> (* decrement *)
    let var = gen_access ~address:true x.node f ibuilder table in 
    let value = Llvm.build_load var ""  ibuilder in 
    (Llvm.build_sub value (Llvm.const_int ll_int 1) "" ibuilder |> 
    Llvm.build_store) var ibuilder |> ignore; value

(** Code-generator for calls;
 *  converts each array argument to a pointer to the values in the array;
 *  then generates a call instruction;   
 *)
and gen_call f_call params f_ctx ibuilder table = 
  let ll_f = lookup f_call table in 
  let actuals = Array.of_list params in 
  let formals = Llvm.params ll_f in 
  let ll_params = 
    Array.map2 (fun x y-> gen_expr_param x y f_ctx ibuilder table ) actuals formals in 
  Llvm.build_call ll_f ll_params "" ibuilder  

(** Code-generator for expressions as parameters:
 *  as gen_expr, but if a parameter is an array, converts it to the formal argument's type;
 *  note: may also build bitcast instructions on incompatible types, as Clang does; i.e.
 *        bitcast [1 x i32]* to [10 x i32]*
 *)
and gen_expr_param actual formal f ibuilder table = 
  let t_formal = Llvm.type_of formal in 
  let ll_actual = gen_expr ~t_null:t_formal actual f ibuilder table in (* address of some value *)
  match element_type ll_actual with
  | Llvm.TypeKind.Array -> (* array as parameter *)
    if  element_type formal = Llvm.TypeKind.Array &&
        Llvm.array_length (Llvm.element_type t_formal) > Llvm.array_length (Llvm.type_of ll_actual |> Llvm.element_type) then 
        emit_warning ("incompatible pointer types passing '"^
        (Llvm.string_of_lltype (Llvm.type_of ll_actual))^"' to parameter of type '"^
        (Llvm.string_of_lltype t_formal)^"'") None; 
    Llvm.build_bitcast ll_actual t_formal "" ibuilder
  | Llvm.TypeKind.Pointer (* leave it as it is *)
  | _ -> ll_actual

(** Code-generator for while statement:
 *  creates basic blocks for the guard expression, 
 *  for the body and for the continuation of the control-flow;
 *  then fill them with instructions and jump instructions;
 *)
and gen_while e s f ibuilder table = 

  (* generates basic blocks *)
  let bb_guard = Llvm.append_block context "" f in
  let bb_body = Llvm.append_block context "" f in 
  let bb_exit = Llvm.append_block context "" f in

  (* unconditional branch to the guard basic block *)
  Llvm.build_br bb_guard ibuilder |> ignore;

  (* generates code for the guard and for the conditional branch *)
  Llvm.position_at_end bb_guard ibuilder;
  let ll_e = gen_expr e f ibuilder table in 
  Llvm.build_cond_br ll_e bb_body bb_exit ibuilder |> ignore;

  (* generates code for the body and unconditional branch to the guard *)
  Llvm.position_at_end bb_body ibuilder;
  gen_stmt s f ibuilder table |> ignore;

  (* if not returned into the statements, create br instruction *)
  if not (there_is_terminator (Llvm.insertion_block ibuilder)) 
  then Llvm.build_br bb_guard ibuilder |> ignore;
  
  Llvm.position_at_end bb_exit ibuilder |> ignore

(** Code-generator for return statement in the middle of a bb:
 *  if the return argument is None, then the function must return void -> build return void
 *  otherwise, evaluates the return argument and build a return instruction for that value;
 *)
and gen_return e f ibuilder table = (match e with
  | None -> Llvm.build_ret_void ibuilder
  | Some exp -> 
    let ll_e = gen_expr (exp.node) f ibuilder table in 
    Llvm.build_ret ll_e ibuilder)
  |> ignore

(** Code-generator for block of instructions;
 *  creates a new scope into which generates the instructions for statements or declarations;
 *)
and gen_block b f ibuilder table = 
  let new_scope = begin_block table in 
  let ret = 
    List.iter (fun x -> gen_stmtordec (x.node) f ibuilder new_scope) b 
  in end_block new_scope |> ignore; ret
and gen_stmtordec e f ibuilder table = match e with
  | Stmt s -> gen_stmt (s.node) f ibuilder table
  | Dec l -> gen_local_list l f ibuilder table 

(** Code-generator for local variable declaration, with optional initialization; *)
and gen_local_list l f ibuilder table = 
  List.iter (
    fun ((t,i),enode) -> 
      let e = List.map (fun x-> x.node) enode in 
      let ty = type_of t in 
      let ll_v = Llvm.build_alloca ty "" ibuilder in 
      (match e with 
      | [ ] -> ()
      | [x] -> (
        (match t with
          | TypA(_,_) -> (* A[] = {x} *)
            Llvm.build_in_bounds_gep ll_v [| ll_zero; ll_zero |] "" ibuilder
          | _ -> ll_v
        ) |> Llvm.build_store (gen_expr ~t_null:ty x f ibuilder table) ) ibuilder |> ignore
      | list -> (* Can be only array A[n] = {x1,...,xk} note: n can be lt or gt k! *)
        let i = ref 0 in  
        (match t with 
        | TypA(tel,Some n) when n > (List.length list) -> (* fill with zeros *)
          (List.map (fun x -> gen_expr x f ibuilder table) list |>
          fill_with) (Llvm.const_null (type_of tel)) n
        | _ -> List.map (fun x -> gen_expr x f ibuilder table) list
        ) |> 
        List.iter (       (* map each element to llvm code, compute its address *)
          fun el ->               (* and store the ll_value into it *)
            let ll_i = Llvm.const_int ll_int !i in 
            (Llvm.build_in_bounds_gep ll_v [| ll_zero;  ll_i |] "" ibuilder |>
            Llvm.build_store el) ibuilder |> ignore;
            incr i
          )
      ); add_entry i ll_v table |> ignore
  ) l

(** Short-circuit evaluation of && 
 *  Creates basic blocks for the cases the first operand e1 is true or false;
 *  generates instructions for the first operand. 
 *  Then branch to bb_true if e1 is true, to bb_false otherwise.
 *  In bb_true, generates code for the second operand, then jumps to bb_false;
 *  In bb_false, merge the two branches using the phi function:
 *  if the previous bb was the "entry" bb, then return false, else return the value of e2;
 *)
and build_shortcircuit_and e1 e2 f ibuilder table = 
  let bb_true = Llvm.append_block context "" f in 
  let bb_false = Llvm.append_block context "" f in 

  (* generate code for e1 and build the conditional jump *)
  let res1 = gen_expr e1 f ibuilder table in 
  let bb1 = Llvm.insertion_block ibuilder in
  Llvm.build_cond_br res1 bb_true bb_false ibuilder |> ignore;
  
  (* if the first argument is true, evaluates the second one, then unconditional branch *)
  Llvm.position_at_end bb_true ibuilder;
  let res2 = gen_expr e2 f ibuilder table in
  let bb2 = Llvm.insertion_block ibuilder in
  Llvm.build_br bb_false ibuilder |> ignore;

  (* merge the two branches using phi function *)
  Llvm.position_at_end bb_false ibuilder;
  Llvm.build_phi [(res1, bb1); (res2, bb2)] "" ibuilder

(** Short-circuit evaluation of ||    
 *  Creates basic blocks for the cases the first operand e1 is true or false;
 *  Generates instructions for the first operand. 
 *  Then branch to bb_true if e1 is true, to bb_false otherwise.
 *  In bb_false, generates code for the second operand, then jumps to bb_true
 *  In bb_true, merge the two branches using the phi function:
 *  if the previous bb was the "entry" bb, then return true, else return the value of e2;
*)
and build_shortcircuit_or e1 e2 f ibuilder table = 
  let bb_true = Llvm.append_block context "" f in 
  let bb_false = Llvm.append_block context "" f in 

  (* generate code for e1 and build the conditional jump *)
  let res1 = gen_expr e1 f ibuilder table in
  let bb1 = Llvm.insertion_block ibuilder in  
  Llvm.build_cond_br res1 bb_true bb_false ibuilder |> ignore;

  (* if the first argument is false, evaluates the second one, then unconditional branch *)
  Llvm.position_at_end bb_false ibuilder;
  let res2 = gen_expr e2 f ibuilder table in
  let bb2 = Llvm.insertion_block ibuilder in 
  Llvm.build_br bb_true ibuilder |> ignore;

  (* merge the two branches using phi function *)
  Llvm.position_at_end bb_true ibuilder;
  Llvm.build_phi [(res1, bb1); (res2, bb2)] "" ibuilder

(** Creates the symbol table and adds the built-in functions;
 *  then maps an AST into an LLVM module and returns it *)
let to_llvm_module p = 
  empty_table |>
  begin_block |> 
  add_entry "print" print |>
  add_entry "getint" getint |>
  gen_prog p; llvm_module