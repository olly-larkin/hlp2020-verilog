module Verishot.Megafunctions.Types

open Verishot.Simulator.Types

type StateVar =
    | EmptyState
    | WireValState of WireVal
    | RAMState of WireVal array * int
