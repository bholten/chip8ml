open Stdint

type t = uint16 array

let create () =
  Array.make 16 Uint16.zero

let write_uint16 t n data =
  let n' = Uint8.to_int n in
  t.(n') <- data

let write_uint8 t n data =
  let n' = Uint8.to_int n in
  t.(n') <- (Uint16.of_uint8 data)

let read_uint8 t n = t.(Uint8.to_int n)
