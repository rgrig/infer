open Printf (* output is meant to be parsed, so avoid Format *)

module L = IList
module N = Cfg.Node
module P = Cfg.Procdesc

let pp_option pp oc = function
  | None -> fprintf oc "(None)"
  | Some x -> fprintf oc "(Some %a)" pp x

let rec pp_list pp oc = function
  | x :: xs -> fprintf oc "(Cons %a %a)" pp x (pp_list pp) xs
  | [] -> fprintf oc "(Nil)"

let pp_2 p1 p2 oc (x1, x2) =
  fprintf oc "(%a %a)" p1 x1 p2 x2

let pp_bool oc = function
  | false -> fprintf oc "false"
  | true -> fprintf oc "true"

let pp_int oc =
  fprintf oc "%d"

let pp_ident oc i =
  fprintf oc "%s" (Ident.to_string i)

let pp_typ oc = function
  | _ -> fprintf oc "SOME_TYP"

let pp_unop oc = function
  | _ -> fprintf oc "SOME_UNOP"

let pp_ifkind oc = function
  | _ -> fprintf oc "SOME_IFKIND"

let rec pp_exp oc = function
  | Sil.Var x ->
      fprintf oc "(Var %a)" pp_ident x
  | Sil.UnOp (op, e, t) ->
      fprintf oc "(UnOp %a %a %a)" pp_unop op pp_exp e (pp_option pp_typ) t
(*
  | BinOp of binop * exp * exp
  | Const of const
  | Cast of typ * exp
  | Lvar of Pvar.t
  | Lfield of exp * Ident.fieldname * typ
  | Lindex of exp * exp
  | Sizeof of typ * Subtype.t
*)
  | _ -> fprintf oc "SOME_EXP"

let pp_typ oc _ =
  fprintf oc "SOME_TYP"

let pp_loc oc _ =
  fprintf oc "SOME_LOCATION"

let pp_call_flags oc _ =
  fprintf oc "SOME_CALL_FLAGS"

let pp_pvar oc _ =
  fprintf oc "SOME_PVAR"

let pp_stackop oc _ =
  fprintf oc "SOME_STACKOP"

let pp_instr oc = function
  | Sil.Letderef (x, e, t, l) ->
      fprintf oc "(Letderef %a %a %a %a)" pp_ident x pp_exp e pp_typ t pp_loc l
  | Sil.Set (e1, t, e2, l) ->
      fprintf oc "(Set %a %a %a %a)" pp_exp e1 pp_typ t pp_exp e2 pp_loc l
  | Sil.Prune (e, l, b, k) ->
      fprintf oc "(Prune %a %a %a %a)" pp_exp e pp_loc l pp_bool b pp_ifkind k
  | Sil.Call (xs, e, ets, l, f) ->
      fprintf oc "(Call %a %a %a %a %a)"
        (pp_list pp_ident) xs
        pp_exp e
        (pp_list (pp_2 pp_exp pp_typ)) ets
        pp_loc l
        pp_call_flags f
  | Sil.Nullify (x, l, b) ->
      fprintf oc "(Nullify %a %a %a)" pp_pvar x pp_loc l pp_bool b
  | Sil.Abstract l ->
      fprintf oc "(Abstract %a)" pp_loc l
  | Sil.Remove_temps (xs, l) ->
      fprintf oc "(Remove_temps %a %a)" (pp_list pp_ident) xs pp_loc l
  | Sil.Stackop (op, l) ->
      fprintf oc "(Stackop %a %a)" pp_stackop op pp_loc l
  | Sil.Declare_locals (pts, l) ->
      fprintf oc "(Declare_locals %a %a)"
        (pp_list (pp_2 pp_pvar pp_typ)) pts
        pp_loc l

let pp_node oc n =
  let cs = N.get_instrs n in
  let ns = N.get_succs n in
  let pp_instr = fprintf oc "    %a\n" pp_instr in
  let pp_id oc n = pp_int oc (N.get_id n :> int) in
  fprintf oc "  label %a:\n" pp_id n;
  List.iter pp_instr cs;
  (match N.get_kind n with
  | N.Exit_node _ ->
      fprintf oc "    (Return)\n\n"
  | _ ->
      fprintf oc "    (Goto %a)\n\n" (pp_list pp_id) ns)

let sort_nodes ns = ns (* TODO: also, make sure start node comes first  *)

let pp_proc oc p =
  let ns = sort_nodes (P.get_nodes p) in
  fprintf oc "procedure %s {\n" (Procname.to_unique_id (P.get_proc_name p));
  List.iter (pp_node oc) ns;
  fprintf oc "}\n\n"

let sort_procs ps = ps (* TODO: by name?  *)

let pp_cfg_to_file filename cfg =
  let filename = DB.filename_add_suffix filename ".pp" in
  let oc = open_out (DB.filename_to_string filename) in
  let ps = sort_procs (Cfg.get_all_procs cfg) in
  List.iter (pp_proc oc) ps;
  flush oc; close_out oc

let store_cfg_to_file filename save_sources cfg =
  Cfg.store_cfg_to_file filename save_sources cfg;
  pp_cfg_to_file filename cfg
