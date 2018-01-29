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
type nonhidden_arc =
  { nh_probability : float
  ; nh_letter : string
  ; nh_target : vertex (* tracks the mc target before product *) }
type product = nonhidden_arc digraph
type monitor =
  { mon_mc : product
  ; mon_decide_yes : vertex list
  ; mon_decide_no : vertex list }

val size : 'a digraph -> int

val product : dfa -> mc -> monitor
val cost_seeall : (*filename*) string -> (*comment*) string -> monitor -> unit
val cost_optim : (*filename*) string -> (*comment*) string -> monitor -> unit
val mc_of_calls : Paths.path_calls -> mc (* probabilities are random *)

val load_dfa : string -> dfa option

val string_of_label : Paths.edge_label -> string
