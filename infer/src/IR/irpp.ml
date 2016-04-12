open Printf (* output is meant to be parsed, so avoid Format *)

module L = IList
module N = Cfg.Node

let pp_ident oc i =
  fprintf oc "%s" (Ident.to_string i)

let pp_exp oc = function
  | Sil.Var x -> pp_ident oc x
(*
  | UnOp of unop * exp * typ option
  | BinOp of binop * exp * exp
  | Const of const
  | Cast of typ * exp
  | Lvar of Pvar.t
  | Lfield of exp * Ident.fieldname * typ
  | Lindex of exp * exp
  | Sizeof of typ * Subtype.t
*)
  | _ -> fprintf oc "SOME_EXP"

let pp_type oc _ =
  fprintf oc "SOME_TYP"

let pp_loc oc _ =
  fprintf oc "SOME_LOCATION"


let pp_instr oc = function
  | Sil.Letderef (x, e, t, l) ->
      fprintf oc "  let %a = *%a:%a # %a\n" pp_ident x pp_exp e pp_type t pp_loc l
(*
  | Set of exp * typ * exp * Location.t
  | Prune of exp * Location.t * bool * if_kind
  | Call of Ident.t list * exp * (exp * typ) list * Location.t * call_flags
  | Nullify of Pvar.t * Location.t * bool
  | Abstract of Location.t (** apply abstraction *)
  | Remove_temps of Ident.t list * Location.t (** remove temporaries *)
  | Stackop of stackop * Location.t (** operation on the stack of propsets *)
  | Declare_locals of (Pvar.t * typ) list * Location.t (** declare local variables *)
  | Goto_node of exp * Location.t
*)
  | _ -> fprintf oc "  unknown\n"

let pp_node oc n =
  fprintf oc "label %d:\n" (N.get_id n);
  L.iter (pp_instr oc) (N.get_instrs n);
  fprintf oc "\n"

let pp_cfg_to_file filename cfg =
  let filename = DB.filename_add_suffix filename ".pp" in
  let oc = open_out (DB.filename_to_string filename) in
  L.iter (pp_node oc) (N.get_all_nodes cfg);
  flush oc; close_out oc

let store_cfg_to_file filename save_sources cfg =
  Cfg.store_cfg_to_file filename save_sources cfg;
  pp_cfg_to_file filename cfg
