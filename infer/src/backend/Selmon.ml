module L = Logging

type vertex = int
type probability = float
type letter = string
type guard = Str.regexp * bool (* re, possibly negated *)

type 'a arc = { arc_label : 'a ; arc_target : vertex }
type 'a digraph = 'a arc list Int.Table.t
type dfa = guard digraph * (* final *) vertex
type mc = (probability * letter) digraph

let product _m _a =
  L.(die InternalError) "todo"

let cost_seeall _m =
  L.(die InternalError) "todo"

let cost_optim _m =
  L.(die InternalError) "todo"

let mc_of_calls _calls =
  L.(die InternalError) "todo"

let load_monitor _filename =
  L.(die InternalError) "todo"

