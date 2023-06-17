open Stdint

type t = {
    display_buffer : Display.t;
    keyboard_buffer : Keyboard.t;
    memory : Memory.t;
    registers : Registers.t;
    stack : Stack.t;
    mutable i : uint16;
    mutable pc : uint16;
    mutable sp : uint8;
    mutable delay_timer : uint8;
    mutable sound_timer : uint8;
  }

type cycle

val execute_instruction : t -> Instructions.t -> cycle
val create : unit -> t
val tick : t -> unit
