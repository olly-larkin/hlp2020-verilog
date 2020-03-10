module Verishot.Test.Util

open Expecto

// uncurry operators
let (||||>) (a, b, c, d) f = f a b c d
let (|||||>) (a, b, c, d, e) f = f a b c d e

let processIntoAsyncTestListMaster = 
    fun (name, got, exp) -> 
        testCaseAsync name <| async { Expect.equal got exp name }

let processIntoAsyncTestList1 f = 
    (fun (name, inp, exp) -> 
        name, (inp |> f), exp) 
        >> processIntoAsyncTestListMaster

let processIntoAsyncTestList2 f = 
    (fun (name, inp, exp) -> 
        name, (inp ||> f), exp) 
        >> processIntoAsyncTestListMaster

let processIntoAsyncTestList3 f = 
    (fun (name, inp, exp) -> 
        name, (inp |||> f), exp) 
        >> processIntoAsyncTestListMaster

let processIntoAsyncTestList4 f = 
    (fun (name, inp, exp) -> 
        name, (inp ||||> f), exp) 
        >> processIntoAsyncTestListMaster
        
let processIntoAsyncTestList5 f = 
    (fun (name, inp, exp) -> 
        name, (inp |||||> f), exp) 
        >> processIntoAsyncTestListMaster
  
let processIntoAsyncTestListE1 f =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.throws (fun _ -> inp |> f |> ignore) exp }   

let processIntoAsyncTestListE2 f =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.throws (fun _ -> inp ||> f |> ignore) exp }

let processIntoAsyncTestListE3 f =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.throws (fun _ -> inp |||> f |> ignore) exp }

let processIntoAsyncTestListE4 f =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.throws (fun _ -> inp ||||> f |> ignore) exp }
