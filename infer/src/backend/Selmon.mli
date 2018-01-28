(** Some experiments with selective monitoring. *)

type vertex = int
type probability = float
type letter = string
type guard = Str.regexp * bool (* re, possibly negated *)

val initial_vertex : vertex

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph
type monitor =
  { mon_mc : mc
  ; mon_decide_yes : vertex list
  ; mon_decide_no : vertex list }

val size : 'a digraph -> int

val product : dfa -> mc -> monitor
val cost_seeall : monitor -> float
val cost_optim : monitor -> float
val mc_of_calls : Paths.path_calls -> mc (* probabilities are random *)

val load_dfa : string -> dfa option

val string_of_label : Paths.edge_label -> string
