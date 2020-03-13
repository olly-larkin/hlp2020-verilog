module Verishot.Megafunctions

open Verishot.CoreTypes.Simulator

type StateVar =
    | EmptyState
    | DFFState of WireVal
    | MyModuleState of string
