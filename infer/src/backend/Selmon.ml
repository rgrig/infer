open !IStd
module L = Logging

type vertex = int
type probability = float
type letter = string
type guard = Str.regexp * bool (* true = must match; false = must not match *)

let initial_vertex = 0

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph

let make_next_id () =
  let n = ref initial_vertex in
  (fun () -> incr n; !n)

let eval_guard (regexp, pos) (_probability, letter) =
  Bool.equal pos (Str.string_match regexp letter 0)

let product ((dfa, _dfa_final) : dfa) (markov_chain : mc) : mc =
  let state : (vertex * vertex) -> vertex =
    let next_id = make_next_id () in
    let module IP = struct
      type t = int * int [@@deriving compare,sexp_of]
      let hash (x, y) = x + y (* TODO: @@deriving hash *)
    end in
    let cache = Hashtbl.create (module IP) () in
    Hashtbl.set cache ~key:(initial_vertex, initial_vertex) ~data:initial_vertex;
    (fun sq -> Hashtbl.find_or_add cache ~default:next_id sq) in
  let result = Int.Table.create () in
  let rec go (s, q) =
    let sq = state (s, q) in
    if not (Hashtbl.mem result sq) then begin
      Hashtbl.set result ~key:sq ~data:[]; (* mark it *)
      let s_out = Hashtbl.find_or_add markov_chain ~default:(fun ()->[]) s in
      let q_out = Hashtbl.find_or_add dfa ~default:(fun ()->[]) q in
      let sq_pairs = List.cartesian_product s_out q_out in
      let maybe_trans (m, a) = if eval_guard a.arc_label m.arc_label then begin
        let sq_target = (m.arc_target, a.arc_target) in
        go sq_target;
        [ { arc_label = m.arc_label; arc_target = state sq_target } ]
      end else [] in
      let sq_trans = List.concat_map ~f:maybe_trans sq_pairs in
      Hashtbl.set result ~key:sq ~data:sq_trans
    end in
  go (initial_vertex, initial_vertex); result
  (* TODO: minimization goes here *)

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
    let next_id = make_next_id () in
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

