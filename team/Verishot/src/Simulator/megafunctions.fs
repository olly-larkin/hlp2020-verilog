module Verishot.Megafunctions

open Verishot.Simulator.Types

type StateVar =
    | EmptyState
    | DFFState of WireVal
    | MyModuleState of string
