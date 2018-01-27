open !IStd
module L = Logging

type vertex = int
type probability = float
type letter = string [@deriving compare,sexp_of]
type guard = Str.regexp * bool (* true = must match; false = must not match *)

let initial_vertex = 0

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph

let make_next_id last =
  let n = ref last in
  (fun () -> incr n; !n)

let eval_guard (regexp, pos) (_probability, letter) =
  Bool.equal pos (Str.string_match regexp letter 0)

let size g =
  let vertex_size ~key:_ ~data:arcs zero = zero + 1 + List.length arcs in
  Hashtbl.fold ~init:0 ~f:vertex_size g

(* TODO: the treatment of initial_vertex is super-annoying *)
let minimize_product markov_chain dfa_final pair =
  assert (Int.equal initial_vertex 0); (* fails badly otherwise :( *)
  let is_final x =
    let _, q = pair x in
    Int.equal q dfa_final in
  let initial_classes =
    (* CONTINUE HERE: initial partition must distinguish by markov component of state *)
    let h = Int.Table.create () in
    let a, b =
      assert (not (Int.equal initial_vertex 1));
      if is_final initial_vertex
      then (initial_vertex, 1)
      else (1, initial_vertex) in
    let f x = Hashtbl.set h ~key:x ~data:(if is_final x then a else b) in
    Hashtbl.iter_keys ~f markov_chain; h in
  let rec loop (old_classes : int Int.Table.t) =
    let next_class = make_next_id (-1) in
    let new_classes = Int.Table.create () in
    let module SIL = struct
      type t = (string * int) list [@@deriving compare,sexp_of]
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
      let nc = Hashtbl.find_or_add by_targets ~default:next_class ts in
      Hashtbl.set new_classes ~key:x ~data:nc in
    (* make sure its class is initial_vertex *)
    do_vertex ~key:initial_vertex ~data:(Hashtbl.find_exn markov_chain initial_vertex);
    Hashtbl.iteri ~f:do_vertex markov_chain;
    let same =
      let xs = List.sort ~cmp:Int.compare (Hashtbl.keys old_classes) in
      let ys = List.sort ~cmp:Int.compare (Hashtbl.keys new_classes) in
      assert (List.equal ~equal:Int.equal xs ys);
      let h = Int.Table.create () in
      let f x =
        let co = Hashtbl.find_exn old_classes x in
        let cn = Hashtbl.find_exn new_classes x in
        (match Hashtbl.find h co with
        | Some cn2 -> Int.equal cn cn2
        | None -> Hashtbl.set h ~key:co ~data:cn; true) in
      List.for_all ~f xs in
    if same then old_classes else loop new_classes in
  let classes = loop initial_classes in
  assert (Option.equal Int.equal
    (Hashtbl.find classes initial_vertex) (Some initial_vertex));
  let small_mc = Int.Table.create () in
  let min_vertex ~key:x ~data:arcs =
    let nx = Hashtbl.find_exn classes x in
    if not (Hashtbl.mem small_mc nx) then begin
      let min_arc { arc_label; arc_target } =
        { arc_label; arc_target = Hashtbl.find_exn classes arc_target } in
      let narcs = List.map ~f:min_arc arcs in
      Hashtbl.set small_mc ~key:nx ~data:narcs
    end in
  Hashtbl.iteri ~f:min_vertex markov_chain;
  small_mc

let product ((dfa, dfa_final) : dfa) (markov_chain : mc) : mc =
  let
    (new_state : (vertex * vertex) -> vertex),
    (old_state : vertex -> (vertex * vertex))
  =
    let next_id = make_next_id initial_vertex in
    let module IP = struct
      type t = int * int [@@deriving compare,sexp_of]
      let hash (x, y) = 31 * Int.hash x + Int.hash y (* TODO: @@deriving hash *)
    end in
    let new_of_old = Hashtbl.create (module IP) () in
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

let cost_optim _m =
  L.(die InternalError) "todo"

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

let load_monitor _filename =
  (* TODO *)
  let dfa = Int.Table.create () in
  Hashtbl.set dfa
    ~key:initial_vertex
    ~data:
      [ { arc_label = (Str.regexp ".*", true)
      ; arc_target = initial_vertex } ];
  Some (dfa, initial_vertex)

