type t = bool array array

let character_set = [|
    0xf0; 0x90; 0x90; 0x90; 0xf0;
    0x20; 0x60; 0x20; 0x20; 0x70;
    0xf0; 0x10; 0xf0; 0x80; 0xf0;
    0xf0; 0x10; 0xf0; 0x10; 0xf0;
    0x90; 0x90; 0xf0; 0x10; 0x10;
    0xf0; 0x80; 0xf0; 0x10; 0xf0;
    0xf0; 0x80; 0xf0; 0x90; 0xf0;
    0xf0; 0x10; 0x20; 0x40; 0x40;
    0xf0; 0x90; 0xf0; 0x90; 0xf0;
    0xf0; 0x90; 0xf0; 0x10; 0xf0;
    0xf0; 0x90; 0xf0; 0x90; 0x90;
    0xe0; 0x90; 0xe0; 0x90; 0xe0;
    0xf0; 0x80; 0x80; 0x80; 0xf0;
    0xf0; 0x90; 0x90; 0x90; 0xe0;
    0xf0; 0x80; 0xf0; 0x80; 0xf0;
    0xf0; 0x80; 0xf0; 0x80; 0x80
  |]

let create () = Array.make_matrix 32 64 false

let clear (s : t) =
  for x = 0 to 63 do
    for y = 0 to 31 do
      s.(y).(x) <- false
    done
  done

let is_set (s : t) (x : int) (y : int) =
  s.(y).(x)

let set (s : t) (x : int) (y : int) =
  s.(y).(x) <- true

let set_b (s : t) (x : int) (y : int) (b : bool) =
  s.(y).(x) <- b
