open Stdint

type register =
  | V0
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | V9
  | VA
  | VB
  | VC
  | VD
  | VE
  | VF

val register_to_int : register -> int
val register_of_int : int -> register

type t

val create : unit -> t
val get : t -> register -> uint8
val set :  t -> register -> uint8 -> unit
val incr : t -> register -> uint8 -> unit
val decr : t -> register -> uint8 -> unit
