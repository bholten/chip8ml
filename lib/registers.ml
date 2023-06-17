open Stdint

type register =
  | V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7
  | V8 | V9 | VA | VB | VC | VD | VE | VF

let register_to_int = function
  | V0 -> 0x00
  | V1 -> 0x01
  | V2 -> 0x02
  | V3 -> 0x03
  | V4 -> 0x04
  | V5 -> 0x05
  | V6 -> 0x06
  | V7 -> 0x07
  | V8 -> 0x08
  | V9 -> 0x09
  | VA -> 0x0A
  | VB -> 0x0B
  | VC -> 0x0C
  | VD -> 0x0D
  | VE -> 0x0E
  | VF -> 0x0F

let register_of_int = function
  | 0x00 -> V0
  | 0x01 -> V1
  | 0x02 -> V2
  | 0x03 -> V3
  | 0x04 -> V4
  | 0x05 -> V5
  | 0x06 -> V6
  | 0x07 -> V7
  | 0x08 -> V8
  | 0x09 -> V9
  | 0x0A -> VA
  | 0x0B -> VB
  | 0x0C -> VC
  | 0x0D -> VD
  | 0x0E -> VE
  | 0x0F -> VF
  | _ -> failwith "Invalid register"

type t = uint8 array

let create () = Array.make 16 Uint8.zero

let get t r =
  register_to_int r |> Array.get t

let set t r n =
  let code = register_to_int r in
  t.(code) <- n

let incr t r n =
  let code = register_to_int r in
  t.(code) <- Uint8.add t.(code) n

let decr t r n =
  let code = register_to_int r in
  t.(code) <- Uint8.sub t.(code) n
