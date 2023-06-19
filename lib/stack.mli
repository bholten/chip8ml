open Stdint

type t

val create : unit -> t
val write_uint16 : t -> uint8 -> uint16 -> unit
val write_uint8 : t -> uint8 -> uint8 -> unit
val read_uint8 : t -> uint8 -> uint16
