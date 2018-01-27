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

val size : 'a digraph -> int

val product : dfa -> mc -> mc
val cost_seeall : mc -> float
val cost_optim : mc -> float
val mc_of_calls : Paths.path_calls -> mc (* probabilities are random *)

val load_monitor : string -> dfa option

val string_of_label : Paths.edge_label -> string
