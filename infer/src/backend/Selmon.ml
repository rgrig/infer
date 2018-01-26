open !IStd
module L = Logging

type vertex = int
type probability = float
type letter = string
type guard = Str.regexp * bool (* re, possibly negated *)

let initial_vertex = 0

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph


let eval_guard (regexp, neg) (_probability, letter) =
  not (Bool.equal neg (Str.string_match regexp letter 0))

let product ((dfa, _dfa_final) : dfa) (markov_chain : mc) : mc =
  let state : (vertex * vertex) -> vertex =
    let next_id : unit -> int =
      let n = ref initial_vertex in
      (fun () -> incr n; !n) in
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

let cost_seeall _m =
  L.(die InternalError) "todo"

let cost_optim _m =
  L.(die InternalError) "todo"

let mc_of_calls _calls =
  (* CONTINUE HERE *)
  L.(die InternalError) "todo"

let load_monitor _filename =
  (* TODO *)
  let dfa = Int.Table.create () in
  Hashtbl.set dfa
    ~key:initial_vertex
    ~data:
      [ { arc_label = (Str.regexp ".*", true)
      ; arc_target = initial_vertex } ];
  Some (dfa, initial_vertex)

