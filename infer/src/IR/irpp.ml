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

let pp_3 p1 p2 p3 oc (x1, x2, x3) =
  fprintf oc "(%a %a %a)" p1 x1 p2 x2 p3 x3

let pp_bool oc = function
  | false -> fprintf oc "false"
  | true -> fprintf oc "true"

let pp_int oc =
  fprintf oc "%d"

let pp_ident oc i =
  fprintf oc "%s" (Ident.to_string i)

let pp_typename oc t =
  fprintf oc "%s" (Typename.to_string t)

let pp_unop oc = function
  | Sil.Neg  -> fprintf oc "Neg"
  | Sil.BNot -> fprintf oc "BNot"
  | Sil.LNot -> fprintf oc "LNot"

let pp_binop oc = function
  | _ -> fprintf oc "SOME_BINOP"

let pp_ifkind oc = function
  | Sil.Ik_bexp -> fprintf oc "Ik_bexp"
  | Sil.Ik_dowhile -> fprintf oc "Ik_dowhile"
  | Sil.Ik_for -> fprintf oc "Ik_for"
  | Sil.Ik_if -> fprintf oc "Ik_if"
  | Sil.Ik_land_lor -> fprintf oc "Ik_land_lor"
  | Sil.Ik_while -> fprintf oc "Ik_while"
  | Sil.Ik_switch -> fprintf oc "Ik_switch"

let pp_ikind oc = function
  | Sil.IChar -> fprintf oc "IChar"
  | Sil.ISChar -> fprintf oc "ISChar"
  | Sil.IUChar -> fprintf oc "IUChar"
  | Sil.IBool -> fprintf oc "IBool"
  | Sil.IInt -> fprintf oc "IInt"
  | Sil.IUInt -> fprintf oc "IUInt"
  | Sil.IShort -> fprintf oc "IShort"
  | Sil.IUShort -> fprintf oc "IUShort"
  | Sil.ILong -> fprintf oc "ILong"
  | Sil.IULong -> fprintf oc "IULong"
  | Sil.ILongLong -> fprintf oc "ILongLong"
  | Sil.IULongLong -> fprintf oc "IULongLong"
  | Sil.I128 -> fprintf oc "I128"
  | Sil.IU128 -> fprintf oc "IU128"

let pp_fkind oc = function
  | _ -> fprintf oc "SOME_FKIND"

let pp_ptr_kind oc = function
  | Sil.Pk_pointer -> fprintf oc "Pk_pointer"
  | Sil.Pk_reference -> fprintf oc "Pk_reference"
  | Sil.Pk_objc_weak -> fprintf oc "Pk_objc_weak"
  | Sil.Pk_objc_unsafe_unretained -> fprintf oc "Pk_objc_unsafe_unretained"
  | Sil.Pk_objc_autoreleasing -> fprintf oc "Pk_objc_autoreleasing"

let pp_csu_t oc = function
  | Csu.Class Csu.CPP -> fprintf oc "(Class CPP)"
  | Csu.Class Csu.Java -> fprintf oc "(Class Java)"
  | Csu.Class Csu.Objc -> fprintf oc "(Class Objc)"
  | Csu.Struct -> fprintf oc "(Struct)"
  | Csu.Union -> fprintf oc "(Union)"
  | Csu.Protocol -> fprintf oc "(Protocol)"

let pp_mangled_t oc m =
  fprintf oc "%s" (Mangled.to_string m)

let pp_item_annotation oc _ =
  fprintf oc "SOME_ITEM_ANNOTATION"

(* XXX: make sure these are escaped, so they don't print spaces. *)

let pp_pvar oc x =
  fprintf oc "%s" (Pvar.to_string x)

let pp_procname oc f =
  fprintf oc "%s" (Procname.to_unique_id f)

let pp_fieldname oc f =
  fprintf oc "%s" (Ident.fieldname_to_string f)

let pp_subtyp oc st =
  fprintf oc "%s" (Sil.Subtype.subtypes_to_string st)

let pp_string_c oc s =
  fprintf oc "%s" s

let pp_int_t oc x =
  fprintf oc "%s" (Sil.Int.to_string x)

let pp_annotation oc
  { Sil.class_name
  ; parameters }
=
  fprintf oc "(annotation";
  fprintf oc " (class_name %a)" pp_string_c class_name;
  fprintf oc " (parameters %a)" (pp_list pp_string_c) parameters;
  fprintf oc ")"

let pp_item_annotation oc =
  fprintf oc "%a" (pp_list (pp_2 pp_annotation pp_bool))

let rec pp_struct_typ oc
  { Sil.instance_fields
  ; static_fields
  ; csu
  ; struct_name
  ; superclasses
  ; def_methods
  ; struct_annotations }
=
  fprintf oc "(struct_typ";
  fprintf oc " (instance_fields %a)" pp_struct_fields instance_fields;
  fprintf oc " (static_fields %a)" pp_struct_fields static_fields;
  fprintf oc " (csu %a)" pp_csu_t csu;
  fprintf oc " (struct_name %a)" (pp_option pp_mangled_t) struct_name;
  fprintf oc " (superclasses %a)" (pp_list pp_typename) superclasses;
  fprintf oc " (def_methods %a)" (pp_list pp_procname) def_methods;
  fprintf oc " (struct_annotations %a)" pp_item_annotation struct_annotations;
  fprintf oc ")"


and pp_typ oc = function
  | Sil.Tvar t ->
      fprintf oc "(Tvar %a)" pp_typename t
  | Sil.Tint k ->
      fprintf oc "(Tint %a)" pp_ikind k
  | Sil.Tfloat k ->
      fprintf oc "(Tfloat %a)" pp_fkind k
  | Sil.Tvoid ->
      fprintf oc "(Tvoid)"
  | Sil.Tfun b ->
      fprintf oc "(Tfun %a)" pp_bool b
  | Sil.Tptr (t, k) ->
      fprintf oc "(Tptr %a %a)" pp_typ t pp_ptr_kind k
  | Sil.Tstruct k ->
      fprintf oc "(Tstruct %a)" pp_struct_typ k
  | Sil.Tarray (t, e) ->
      fprintf oc "(Tarray %a %a)" pp_typ t pp_exp e

and pp_exp oc = function
  | Sil.Var x ->
      fprintf oc "(Var %a)" pp_ident x
  | Sil.UnOp (op, e, t) ->
      fprintf oc "(UnOp %a %a %a)" pp_unop op pp_exp e (pp_option pp_typ) t
  | Sil.BinOp (op, e1, e2) ->
      fprintf oc "(BinOp %a %a %a)" pp_binop op pp_exp e1 pp_exp e2
  | Sil.Const c ->
      fprintf oc "(Const %a)" pp_const c
  | Sil.Cast (t, e) ->
      fprintf oc "(Cast %a %a)" pp_typ t pp_exp e
  | Sil.Lvar x ->
      fprintf oc "(Lvar %a)" pp_pvar x
  | Sil.Lfield (e, f, t) ->
      fprintf oc "(Lfield %a %a %a)" pp_exp e pp_fieldname f pp_typ t
  | Sil.Lindex (e1, e2) ->
      fprintf oc "(Lindex %a %a)" pp_exp e1 pp_exp e2
  | Sil.Sizeof (t, st) ->
      fprintf oc "(Sizeof %a %a)" pp_typ t pp_subtyp st

and pp_const oc = function
  | Sil.Cint c ->
      fprintf oc "(Cint %a)" pp_int_t c
  | Sil.Cfun f ->
      fprintf oc "(Cfun %a)" pp_procname f
  | Sil.Cstr s ->
      fprintf oc "(Cstr %a)" pp_string_c s
(*
  | Cfloat of float (** float constants *)
  | Cattribute of attribute (** attribute used in disequalities to annotate a value *)
  | Cexn of exp (** exception *)
  | Cclass of Ident.name (** class constant *)
  | Cptr_to_fld of Ident.fieldname * typ (** pointer to field constant,
                                             and type of the surrounding Csu.t type *)
  | Cclosure of closure (** anonymous function *)
*)
  | _ -> fprintf oc "SOME_CONST"

and pp_struct_fields oc =
  fprintf oc "%a" (pp_list (pp_3 pp_fieldname pp_typ pp_item_annotation))

let pp_loc oc l =
  fprintf oc "(Location %s)" (Location.to_string l)

let pp_call_flags oc _ =
  fprintf oc "SOME_CALL_FLAGS"

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

module NodePriorityQueue : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val push : int * N.t -> t -> t
  val pop : t -> N.t * t
  val decrease : N.t -> t -> t
end = struct
  (* NOTE: This implementation is a quick hack. *)
  module M = Cfg.NodeMap
  module S = Set.Make (struct
    type t = int * N.t
    let compare (p, m) (q, n) =
      let c = compare p q in
      if c <> 0 then c else N.compare m n
  end)
  type t = int M.t * S.t

  let empty = (M.empty, S.empty)

  let is_empty (m, s) = M.is_empty m && S.is_empty s

  let push (p, x) (m, s) = (M.add x p m, S.add (p, x) s)

  let pop (m, s) =
    let p, x = S.min_elt s in
    (x, (M.remove x m, S.remove (p, x) s))

  let decrease x (m, s) =
    try
      let p = M.find x m in
      (M.add x (p - 1) m, S.add (p - 1, x) (S.remove (p, x) s))
    with Not_found -> (m, s)
end


let sort_nodes_for_readability ns =
  let module Q = NodePriorityQueue in
  let rec loop ms q =
    if Q.is_empty q then List.rev ms else begin
      let n, q = Q.pop q in
      let q = List.fold_right Q.decrease (N.get_succs n) q in
      loop (n :: ms) q
    end in
  let pns =
    let f n = (List.length (N.get_preds n), n) in
    List.map f ns in
  let q = List.fold_right Q.push pns Q.empty in
  loop [] q

(* Puts start nodes first. Prints a warning if the number of start nodes isn't
exactly 1. May also shuffle the order of other nodes, to match some subjective
notion of "readability". *)
let sort_nodes oc ns =
  let ns = sort_nodes_for_readability ns in
  let is_start n = match N.get_kind n with
    | N.Start_node _ -> 1
    | _ -> 0 in
  let c = List.fold_left (+) 0 (List.map is_start ns) in
  if c <> 1 then fprintf oc "; WARNING: procedure with %d start nodes\n" c;
  let compare m n = compare (is_start n) (is_start m) in
  List.stable_sort compare ns

let pp_proc oc p =
  let ns = sort_nodes oc (P.get_nodes p) in
  fprintf oc "procedure %a {\n" pp_procname (P.get_proc_name p);
  List.iter (pp_node oc) ns;
  fprintf oc "}\n\n"

(* Sort by name, for readability of output. *)
let sort_procs =
  let name p = Procname.get_method (P.get_proc_name p) in
  let compare p q = compare (name p) (name q) in
  List.stable_sort compare

let pp_cfg_to_file filename cfg =
  let filename = DB.filename_add_suffix filename ".pp" in
  let oc = open_out (DB.filename_to_string filename) in
  let ps = sort_procs (Cfg.get_all_procs cfg) in
  List.iter (pp_proc oc) ps;
  flush oc; close_out oc

let store_cfg_to_file filename save_sources cfg =
  Cfg.store_cfg_to_file filename save_sources cfg;
  pp_cfg_to_file filename cfg
