open !IStd
module L = Logging

type vertex = int [@@deriving compare,sexp_of]
type probability = float
type letter = string [@@deriving compare,sexp_of]
type guard = Str.regexp * bool (* true = must match; false = must not match *)

let initial_vertex = 0

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph
type nonhidden_arc =
  { nh_probability : float
  ; nh_letter : string
  ; nh_target : vertex (* tracks the mc target before product *) }
type product = nonhidden_arc digraph
type monitor =
  { mon_mc : product
  ; mon_decide_yes : vertex list
  ; mon_decide_no : vertex list }

let make_next_id last =
  let n = ref last in
  (fun () -> incr n; !n)

let eval_guard (regexp, pos) (_probability, letter) =
  Bool.equal pos (Str.string_match regexp letter 0)

let size g =
  let vertex_size ~key:_ ~data:arcs zero = zero + 1 + List.length arcs in
  Hashtbl.fold ~init:0 ~f:vertex_size g


module IntIntPair = struct
  type t = int * int [@@deriving compare,sexp_of]
  let hash (x, y) = 31 * Int.hash x + Int.hash y (* TODO: @@deriving hash *)
end

module StringIntPair = struct
  type t = string * int [@@ deriving compare,sexp_of]
  let hash (x, y) = 31 * String.hash x + Int.hash y (* TODO: deriving *)
end

type decision = Yes | No | Maybe

let fold_arcs
  (g : 'a digraph)
  ~(init : 'b)
  ~(f : ('b -> source:vertex -> label:'a -> target:vertex -> 'b))
=
  let do_arc source acc { arc_label; arc_target } =
    f acc ~source ~label:arc_label ~target:arc_target in
  let do_vertex ~key:source ~data:outgoing init =
    List.fold ~init ~f:(do_arc source) outgoing in
  Hashtbl.fold g ~init ~f:do_vertex

let iter_arcs
  (g : 'a digraph)
  ~(f : (source:vertex -> label:'a -> target:vertex -> unit))
=
  let f () ~source ~label ~target = f ~source ~label ~target in
  fold_arcs ~init:() ~f g

let find_sccs markov_chain =
  let rev = Int.Table.create () in
  let rev_arc ~source ~label:_ ~target =
    let old = Hashtbl.find_or_add rev ~default:(fun ()->[]) target in
    Hashtbl.set rev ~key:target ~data:(source :: old) in
  iter_arcs ~f:rev_arc markov_chain;
  let seen1 = Int.Hash_set.create () in
  let rec dfs1 rev_post x = if not (Hash_set.mem seen1 x) then begin
    Hash_set.add seen1 x;
    let out = Hashtbl.find_or_add rev ~default:(fun ()->[]) x in
    x :: List.fold ~init:rev_post ~f:dfs1 out
  end else rev_post in
  let rev_post = List.fold ~init:[] ~f:dfs1 (Hashtbl.keys markov_chain) in
  let sccs = Int.Table.create () in
  let rec dfs2 rep x = if not (Hashtbl.mem sccs x) then begin
    Hashtbl.set sccs ~key:x ~data:rep;
    let out = Hashtbl.find_or_add markov_chain ~default:(fun ()->[]) x in
    let f { arc_target; _ } = dfs2 rep arc_target in
    List.iter ~f out
  end in
  let start_dfs2 x = dfs2 x x in
  List.iter ~f:start_dfs2 rev_post;
  sccs

let get_deciding markov_chain is_final =
  let sccs = find_sccs markov_chain in
  let scc_digraph = Int.Table.create () in
  let make_scc_vertex x =
    let c = Hashtbl.find_exn sccs x in
    Hashtbl.set scc_digraph ~key:c ~data:[] in
  Hashtbl.iter_keys ~f:make_scc_vertex markov_chain;
  let make_scc_arc ~source ~label:_ ~target =
    let s_scc = Hashtbl.find_exn sccs source in
    let t_scc = Hashtbl.find_exn sccs target in
    if not (Int.equal s_scc t_scc) then begin (* NOTE: we drop loops *)
      let old = Hashtbl.find_exn scc_digraph s_scc in
      Hashtbl.set scc_digraph ~key:s_scc ~data:(t_scc :: old)
    end in
  iter_arcs ~f:make_scc_arc markov_chain;
  let seen = Int.Hash_set.create () in
  let decision = Int.Table.create () in
  let rec dfs x =
    assert (not (Hashtbl.mem decision x));
    Hash_set.add seen x;
    let do_arc (all_yes, all_no, _some) arc_target =
      let is_s = Hash_set.mem seen arc_target in
      let is_d = Hashtbl.mem decision arc_target in
      if is_s && not is_d
      then L.(die InternalError) "scc_digraph should be acyclic"
      else begin
        if not is_s then dfs arc_target;
        (match Hashtbl.find_exn decision arc_target with
        | Yes -> (all_yes, false, true)
        | No -> (false, all_no, true)
        | Maybe -> (false, false, true))
    end in
    let outgoing = Hashtbl.find_exn scc_digraph x in
    let all_yes, all_no, some =
      List.fold ~init:(true,true,false) ~f:do_arc outgoing in
    let t =
      if some && all_yes then Yes
      else if all_no then No
      else Maybe in
    Hashtbl.set decision ~key:x ~data:t
  in
  let tag_if_final x =
    if is_final x then
      let c = Hashtbl.find_exn sccs x in
      Hashtbl.set decision ~key:c ~data:Yes;
      Hash_set.add seen c
  in
  Hashtbl.iter_keys ~f:tag_if_final markov_chain;
  let maybe_dfs x =
    if not (Hashtbl.mem decision x) then dfs x in
  Hashtbl.iter_keys ~f:maybe_dfs scc_digraph;
  let decide_yes = ref [] in
  let decide_no = ref [] in
  let bin_it x =
    let c = Hashtbl.find_exn sccs x in
    (match Hashtbl.find_exn decision c with
    | Yes -> decide_yes := x :: !decide_yes
    | No -> decide_no := x :: !decide_no
    | _ -> ()) in
  Hashtbl.iter_keys ~f:bin_it markov_chain;
  (!decide_yes, !decide_no)

let dummy_yes =
  { mon_mc = (let h = Int.Table.create () in Hashtbl.set h ~key:0 ~data:[]; h)
  ; mon_decide_yes = [ initial_vertex ]
  ; mon_decide_no = [] }
let dummy_no =
  { mon_mc = (let h = Int.Table.create () in Hashtbl.set h ~key:0 ~data:[]; h)
  ; mon_decide_yes = [ initial_vertex ]
  ; mon_decide_no = [] }

(* NOTE: does not glue if markov component is distinct *)
let minimize_product markov_chain dfa_final pair =
  assert (Int.equal initial_vertex 0); (* fails badly otherwise :( *)
  let keys_of h = List.sort ~cmp:Int.compare (Hashtbl.keys h) in
  let old_vertices = keys_of markov_chain in
  let old_decide_yes, old_decide_no =
    get_deciding markov_chain (fun sq -> Int.equal dfa_final (snd (pair sq))) in
  if List.mem ~equal:Int.equal old_decide_yes 0 then dummy_yes
  else if List.mem ~equal:Int.equal old_decide_no 0 then dummy_no
  else begin
    let check_keys h =
      assert (List.equal ~equal:Int.equal old_vertices (keys_of h)) in
    let initial_classes = (* classes are MCx{maybe,n,y} *)
      let classes = Int.Table.create () in
      let offset = Int.Table.create () in
      let seto o x = Hashtbl.set offset ~key:x ~data:o in
      List.iter ~f:(seto 1) old_decide_no;
      List.iter ~f:(seto 2) old_decide_yes;
      let f sq =
        let s, _ = pair sq in
        let data = 3 * s + (Hashtbl.find_or_add offset ~default:(fun ()->0) sq) in
        Hashtbl.set classes ~key:sq ~data in
      Hashtbl.iter_keys ~f markov_chain; classes in
    assert (Int.equal 0 (Hashtbl.find_exn initial_classes 0));
    let rec loop (old_classes : int Int.Table.t) =
      check_keys old_classes;
      let next_refinement = make_next_id (-1) in
      let refinement = Int.Table.create () in
      let module SIL = struct
        type t = (letter * int) list [@@deriving compare,sexp_of]
        let rec hash = function (* TODO: deriving *)
          | [] -> 0
          | (s, i) :: tail ->
              String.hash s + 31 * (Int.hash i + 31 * hash tail)
      end in
      let by_targets = Hashtbl.create (module SIL) () in
      let do_vertex ~key:x ~data:arcs =
        let ts = String.Table.create () in
        let rec_arc { arc_label = (_prob, l); arc_target } =
          let cls = Hashtbl.find_exn old_classes arc_target in
          Hashtbl.set ts ~key:l ~data:cls in
        List.iter ~f:rec_arc arcs;
        let ts = Hashtbl.fold ~init:[] ~f:(fun ~key ~data xs -> (key,data)::xs) ts in
        let cmp (s1, x1) (s2, x2) =
          let cs = String.compare s1 s2 in
          if Int.equal cs 0 then Int.compare x1 x2 else cs in
        let ts = List.sort ~cmp ts in
        let nc = Hashtbl.find_or_add by_targets ~default:next_refinement ts in
        Hashtbl.set refinement ~key:x ~data:nc in
      Hashtbl.iteri ~f:do_vertex markov_chain;
      check_keys refinement;
      let new_classes =
        let h = Int.Table.create () in
        let g = Hashtbl.create (module IntIntPair) () in
        let cr x = (* old-class and refinement of vertex x *)
          ( Hashtbl.find_exn old_classes x
          , Hashtbl.find_exn refinement x ) in
        let iv_oc, iv_r = cr 0 in
        assert (Int.equal iv_oc 0);
        Hashtbl.set g ~key:(iv_oc,iv_r) ~data:0; (* initial_vertex *)
        let next_class = make_next_id 0 in
        let do_x x =
          let nc = Hashtbl.find_or_add g ~default:next_class (cr x) in
          Hashtbl.set h ~key:x ~data:nc in
        List.iter ~f:do_x old_vertices; h in
      check_keys new_classes;
      let same = (* TODO: simplify *)
        let h = Int.Table.create () in
        let f x =
          let co = Hashtbl.find_exn old_classes x in
          let cn = Hashtbl.find_exn new_classes x in
          (match Hashtbl.find h co with
          | Some cn2 -> Int.equal cn cn2
          | None -> Hashtbl.set h ~key:co ~data:cn; true) in
        List.for_all ~f old_vertices in
      if same then old_classes else loop new_classes in
    let classes = loop initial_classes in
    assert (Option.equal Int.equal
      (Hashtbl.find classes initial_vertex) (Some initial_vertex));
    let small_mc = Int.Table.create () in
    let min_vertex ~key:x ~data:arcs =
      let nx = Hashtbl.find_exn classes x in
      if not (Hashtbl.mem small_mc nx) then begin
        let min_arc { arc_label = (prob, letter); arc_target } =
          let arc_label =
            { nh_probability = prob
            ; nh_letter = letter
            ; nh_target = fst (pair arc_target) } in
          let arc_target = Hashtbl.find_exn classes arc_target in
          { arc_label; arc_target } in
        let narcs = List.map ~f:min_arc arcs in
        Hashtbl.set small_mc ~key:nx ~data:narcs
      end in
    Hashtbl.iteri ~f:min_vertex markov_chain;
    let map_vertices (xs : vertex list) : vertex list = (* image via classes *)
      let h = Int.Hash_set.create () in
      let f x =
        let c = Hashtbl.find_exn classes x in
        Hash_set.add h c in
      List.iter ~f xs;
      Hash_set.to_list h in
    { mon_mc = small_mc
    ; mon_decide_yes = map_vertices old_decide_yes
    ; mon_decide_no = map_vertices old_decide_no }
  end (* the non-dummy case *)

let product ((dfa, dfa_final) : dfa) (markov_chain : mc) : monitor =
  let
    (new_state : (vertex * vertex) -> vertex),
    (old_state : vertex -> (vertex * vertex))
  =
    let next_id = make_next_id initial_vertex in
    let new_of_old = Hashtbl.create (module IntIntPair) () in
    let old_of_new = Int.Table.create () in
    Hashtbl.set new_of_old ~key:(initial_vertex, initial_vertex) ~data:initial_vertex;
    Hashtbl.set old_of_new ~key:initial_vertex ~data:(initial_vertex, initial_vertex);
    let new_state sq = match Hashtbl.find new_of_old sq with
      | Some n_sq -> n_sq
      | None ->
          let n_sq = next_id () in
          Hashtbl.set new_of_old ~key:sq ~data:n_sq;
          Hashtbl.set old_of_new ~key:n_sq ~data:sq;
          n_sq in
    let old_state n_sq = Hashtbl.find_exn old_of_new n_sq in
    (new_state, old_state) in
  let result = Int.Table.create () in
  let all_final = Int.Hash_set.create () in
  let rec go (s, q) =
    let sq = new_state (s, q) in
    if Int.equal q dfa_final then Hash_set.add all_final sq;
    if not (Hashtbl.mem result sq) then begin
      Hashtbl.set result ~key:sq ~data:[]; (* mark it *)
      let s_out = Hashtbl.find_or_add markov_chain ~default:(fun ()->[]) s in
      let q_out = Hashtbl.find_or_add dfa ~default:(fun ()->[]) q in
      let sq_pairs = List.cartesian_product s_out q_out in
      let maybe_trans (m, a) = if eval_guard a.arc_label m.arc_label then begin
        let sq_target = (m.arc_target, a.arc_target) in
        go sq_target;
        [ { arc_label = m.arc_label; arc_target = new_state sq_target } ]
      end else [] in
      let sq_trans = List.concat_map ~f:maybe_trans sq_pairs in
      Hashtbl.set result ~key:sq ~data:sq_trans
    end in
  go (initial_vertex, initial_vertex);
  minimize_product result dfa_final old_state

let cost_seeall _m =
  L.(die InternalError) "todo"

let compute_confused_pairs product =
  (* group arcs in a (nested) map: letter->target->(source list) *)
  let groups = Hashtbl.create (module StringIntPair) () in
  let bin_arc ~source ~label:{ nh_letter; nh_target; _ } ~target =
    let letter = (nh_letter, nh_target) in
    let g = Hashtbl.find_or_add groups ~default:Int.Table.create letter in
    let ss = Hashtbl.find_or_add g ~default:(fun ()->[]) target in
    Hashtbl.set g ~key:target ~data:(source :: ss) in
  iter_arcs ~f:bin_arc product;

  (* for each letter, cross product sources on different targets *)
  let do_two_targets pairs (sources1, sources2) =
    List.unordered_append (List.cartesian_product sources1 sources2) pairs in
  let do_letter pairs (bin : vertex list Int.Table.t) =
    let rec make_pairs ps = function (* builds (n choose 2) pairs *)
      | [] -> ps
      | x :: xs ->
          let ps = List.fold ~init:ps ~f:(fun ps y -> (x,y)::ps) xs in
          make_pairs ps xs in
    let sss = Hashtbl.data bin in
    let ssp : (vertex list * vertex list) list = make_pairs [] sss in
    List.fold ~init:pairs ~f:do_two_targets ssp in
  List.fold ~init:[] ~f:do_letter (Hashtbl.data groups)

let cost_optim { mon_mc; mon_decide_yes; mon_decide_no } =
  let bad_pairs = compute_confused_pairs mon_mc in
  Printf.printf "BAD_PAIR_COUNT %d\n%!" (List.length bad_pairs);
  1.0
(*   L.(die InternalError) "todo" *)

let string_of_label = Paths.(function
  | Epsilon -> "ε"
  | Call proc_name ->
      Printf.sprintf "CALL %s" (Typ.Procname.to_filename proc_name)
  | Return proc_name ->
      Printf.sprintf "RETURN %s" (Typ.Procname.to_filename proc_name))

let mc_of_calls (Paths.{ start_node; edges; _ } : Paths.path_calls) : mc =
  let mc = Int.Table.create () in
  let v = (* TODO: refactor; see product *)
    let next_id = make_next_id initial_vertex in
    let cache = Int.Table.create () in
    Hashtbl.set cache ~key:start_node ~data:initial_vertex;
    (fun x -> Hashtbl.find_or_add cache ~default:next_id x) in
  let do_edge (source, target, label) =
    let arc = { arc_label = string_of_label label; arc_target = v target } in
    let old_arcs = Hashtbl.find_or_add mc ~default:(fun ()->[]) (v source) in
    Hashtbl.set mc ~key:(v source) ~data:(arc :: old_arcs) in
  let add_probabilities arcs =
    let n = List.length arcs in
    let xs = List.init (n - 1) ~f:(fun _ -> Random.float_range 0.0 1.0) in
    let xs = List.sort ~cmp:Float.compare xs in
    let probs = List.map2_exn ~f:(-.) (xs @ [1.0]) (0.0 :: xs) in
    let add_one p { arc_label; arc_target } =
      { arc_label = (p, arc_label); arc_target } in
    List.map2_exn ~f:add_one probs arcs
  in
  List.iter ~f:do_edge edges;
  Hashtbl.map ~f:add_probabilities mc

let load_dfa _filename =
  (* TODO *)
  assert (Int.equal initial_vertex 0);
  let dfa = Int.Table.create () in
  Hashtbl.set dfa
    ~key:0
    ~data:
      [ { arc_label = (Str.regexp "CALL.*next", true); arc_target = 1 }
      ; { arc_label = (Str.regexp "CALL.*next", false); arc_target = 0 } ];
  Hashtbl.set dfa
    ~key:1
    ~data:
      [ { arc_label = (Str.regexp "CALL.*next", true); arc_target = 2 }
      ; { arc_label = (Str.regexp "RET.*hasNext", true); arc_target = 0 }
      ; { arc_label = (Str.regexp "CALL.*next\\|RET.*hasNext", false); arc_target = 1 } ];
  Hashtbl.set dfa
    ~key:2
    ~data:[ { arc_label = (Str.regexp ".*", true); arc_target = 2 } ];
  Some (dfa, 2)

