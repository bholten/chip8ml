open Stdint

type key =
  | K_0
  | K_1
  | K_2
  | K_3
  | K_4
  | K_5
  | K_6
  | K_7
  | K_8
  | K_9
  | K_A
  | K_B
  | K_C
  | K_D
  | K_E
  | K_F

type t = {
    key_states : bool array;
    mutable pressed : int option
  }

let create () = {
    key_states = Array.make 16 false;
    pressed = None
  }

let is_key_pressed_uint8 (t : t) (n : uint8) =
  t.key_states.(Uint8.to_int n)

let key_down (t : t) (n : int)  =
  t.key_states.(n) <- true;
  t.pressed <- Some n

let key_up (t : t) (n : int) =
  t.key_states.(n) <- false;
  t.pressed <- None
