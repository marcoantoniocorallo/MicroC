open Ast

exception DuplicateEntry of identifier

(** A symbol table can be viewed like a chain of maps (var -> info): one for each block
    For simplicity and efficiency, the choice is to use a list of hashmaps. 
    The head of the list is the current block, the inner one. 
    The global scope is the last element of the list. *)
type 'a t = Table of (identifier, 'a) Hashtbl.t list

(** empty_table returns a new empty symbol table, i.e. a list without blocks *)
let empty_table = Table []

(** begin_block t adds a block to the list of blocks of t *)
let begin_block = function
  | Table xs -> Table ((Hashtbl.create 0)::xs)

(** end_block t returns the tail of the list of blocks 
    if the list is empty, it fails! *)
let end_block = function
  | Table (_::xs) -> Table xs | _ -> failwith("Attempt to remove a block from an empty symbol table.")

(** add_entry t v i adds a binding <v,i> to the head of t, if any. Otherwise, it fails. 
    If an entry is already present for v, it raise a DuplicateEntry error. *)
let add_entry var info table = match table with
  | Table (x::_) -> 
    if Hashtbl.mem x var then raise(DuplicateEntry(var))
    else Hashtbl.add x var info; table
  | _ -> failwith("Attempt to add an entry into an empty symbol table.")

(** lookup v t looks for an entry <v,_> in t and returns the first occurrence. 
  * i.e. the instance declared in the outer scope. 
  *)
let lookup var (Table l) = match List.find_opt (fun x -> Hashtbl.mem x var) l with
  | Some block -> Hashtbl.find block var
  | None -> failwith("Error: "^var^" not found in symbol table.")

(** of_alist l creates a new scope from the elements of the list, 
    that are of the form <var, info>. *)
let of_alist l = 
  let ht = Hashtbl.create (List.length l) in
  List.iter (fun (x,y) -> Hashtbl.add ht x y) l;
  Table [ ht ]