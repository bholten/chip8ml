open Stdint

type t = uint16 array

let create () =
  Array.make 16 Uint16.zero

let write_uint8 (t : t) (n : uint8) (data : uint8) =
  let n' = Uint8.to_int n in
  t.(n') <- (Uint16.of_uint8 data)
