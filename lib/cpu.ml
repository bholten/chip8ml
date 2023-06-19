open Stdint
open Instructions

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

let create () = {
    display_buffer = Display.create ();
    keyboard_buffer = Keyboard.create ();
    memory = Memory.create ();
    registers = Registers.create ();
    stack = Stack.create ();
    i = Uint16.zero;
    pc = Uint16.of_int 0x200;
    sp = Uint8.zero;
    delay_timer = Uint8.zero;
    sound_timer = Uint8.zero;
  }

type cycle = NEXT | SKIP | JUMP of uint16

let (mod) x y = ((x mod y) + y) mod y

let execute_instruction t = function
  | CLS ->
     Display.clear t.display_buffer;
     NEXT
  | RET ->
     t.sp <- Uint8.(sub t.sp one);
     t.pc <- Stack.read_uint8 t.stack t.sp;
     NEXT
  | JP_nnn nnn -> JUMP nnn
  | CALL_nnn nnn ->
     Stack.write_uint16 t.stack t.sp t.pc;
     t.sp <- Uint8.(add t.sp one);
     JUMP nnn
  | SE_Vx_nn (vx, nn) ->
     let x = Registers.get t.registers vx in     
     if x = nn
     then SKIP
     else NEXT
  | SNE_Vx_nn (vx, nn) ->
     let x = Registers.get t.registers vx in
     if x <> nn
     then SKIP
     else NEXT
  | SE_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     if x = y
     then SKIP
     else NEXT
  | LD_Vx_nn (vx, nn) ->
     Registers.set t.registers vx nn;
     NEXT
  | ADD_Vx_nn (vx, nn) ->
     Registers.incr t.registers vx nn;
     NEXT
  | LD_Vx_Vy (vx, vy) ->
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx y;
     NEXT
  | OR_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx @@ Uint8.logor x y;
     NEXT
  | AND_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx @@ Uint8.logand x y;
     NEXT
  | XOR_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx @@ Uint8.logxor x y;
     NEXT
  | ADD_Vx_Vy_Vf (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     let sum = (Uint8.to_int x) + (Uint8.to_int y) in
     Registers.set t.registers VF @@ if sum > 0xFF then Uint8.one else Uint8.zero;
     Registers.set t.registers vx @@ Uint8.of_int sum;
     NEXT
  | SUB_Vx_Vy_Vf  (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx Uint8.(sub x y);
     Registers.set t.registers VF @@ if Uint8.compare x y = 1 then Uint8.one else Uint8.zero;
     NEXT
  | SHR_Vy  (vx, vy) ->
     let y = Registers.get t.registers vy in
     let y' = Uint8.shift_right y 1 in
     let lsb = Uint8.(logand y @@ of_int 0b00000001) in
     Registers.set t.registers vx y';
     Registers.set t.registers vy y';
     Registers.set t.registers VF @@ if lsb = Uint8.one then Uint8.one else Uint8.zero;
     NEXT
  | SUBN_Vx_Vy_Vf (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     Registers.set t.registers vx @@ Uint8.sub y x;
     Registers.set t.registers VF @@ if Uint8.compare y x = 1 then Uint8.one else Uint8.zero;
     NEXT
  | SHL_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     let x' = Uint8.shift_left x 1 in
     let y' = Uint8.shift_left y 1 in
     let msb = Uint8.logand y @@ Uint8.of_int 0b10000000 in
     Registers.set t.registers vx x';
     Registers.set t.registers vy y';
     Registers.set t.registers VF @@ if msb = Uint8.zero then Uint8.zero else Uint8.one;
     NEXT
  | SNE_Vx_Vy (vx, vy) ->
     let x = Registers.get t.registers vx in
     let y = Registers.get t.registers vy in
     if x <> y then SKIP
     else NEXT
  | LD_I nnn ->
     t.i <- nnn;
     NEXT
  | JP_V0 nnn ->
     let v0 = Registers.get t.registers V0 |> Uint16.of_uint8 in
     JUMP (Uint16.add nnn v0)
  | RND_Vx_nn (vx, nn) ->
     let r = Random.int 255 |> Uint8.of_int |> Uint8.logand nn in
     Registers.set t.registers vx r;
     NEXT
  | DRW_Vx_Vy_n (vx, vy, n) ->
     (* mod *)
     let x' = Registers.get t.registers vx |> Uint8.to_int in
     let x = Uint8.of_int (x' mod 64) in
     
     let y' = Registers.get t.registers vy |> Uint8.to_int in
     let y = Uint8.of_int (y' mod 32) in
     
     Registers.set t.registers VF Uint8.zero;

     for a = 0 to Uint8.(sub n one |> to_int) do
       let sprite = Memory.read_uint8 t.memory Uint16.(add t.i (of_int a)) in
       let dy = a + (Uint8.to_int y) in

       for b = 0 to 7 do
         let bmsb = Uint8.of_int @@ Int.shift_right 0b10000000 b in
         let s = Uint8.logand sprite bmsb in

         if s <> Uint8.zero then
           let dx = Uint8.(of_int b |> add x |> to_int) in
           let set = Display.is_set t.display_buffer dx dy in
           
           if set then
             Registers.set t.registers VF Uint8.one;

           Display.set_b t.display_buffer dx dy (not set);
       done;
     done;
     NEXT
  | SKP_Vx vx ->
     let x = Registers.get t.registers vx in
     let kp = Keyboard.is_key_pressed_uint8 t.keyboard_buffer x in     
     if kp
     then SKIP
     else NEXT
  | SKNP_Vx vx ->
     let x = Registers.get t.registers vx in
     let kp = Keyboard.is_key_pressed_uint8 t.keyboard_buffer x in
     if not kp
     then SKIP
     else NEXT
  | LD_Vx_Dt vx ->
     Registers.set t.registers vx t.delay_timer;
     NEXT
  | LD_Vx_n (vx, _) -> (* TODO *)
     begin match t.keyboard_buffer.pressed with
     | Some k ->
        Registers.set t.registers vx (Uint8.of_int k);
        NEXT
     | None -> JUMP t.pc
     end
  | LD_Dt_Vx vx ->
     t.delay_timer <- Registers.get t.registers vx;
     NEXT
  | LD_St_Vx vx ->
     t.sound_timer <- Registers.get t.registers vx;
     NEXT
  | ADD_I_Vx vx ->
     let x = Registers.get t.registers vx |> Uint16.of_uint8 in
     t.i <- Uint16.add t.i x;
     NEXT
  | LD_F_Vx vx ->
     let x = Registers.get t.registers vx |> Uint16.of_uint8 in
     let five = Uint16.of_int 5 in
     t.i <- Uint16.mul x five;
     NEXT
  | LD_B_Vx vx ->
     let x = Registers.get t.registers vx |> Uint8.to_int in
     let hundreds = Uint8.of_int @@ x / 100 in
     let tens = Uint8.of_int @@ (x / 10) mod 10 in
     let ones = Uint8.of_int @@ x mod 10 in
     Memory.write_uint8 t.memory t.i hundreds;
     Memory.write_uint8 t.memory Uint16.(add t.i one) tens;
     Memory.write_uint8 t.memory Uint16.(add t.i @@ of_int 2) ones;
     NEXT
  | LD_I_Vx vx ->
     let x = Registers.register_to_int vx in
     for k = 0 to x do
       let k' = Uint16.(add t.i @@ of_int k) in
       let rs = Registers.register_of_int k in
       let r = Registers.get t.registers rs in
       Memory.write_uint8 t.memory k' r
     done;
     NEXT
  | LD_Vx_I vx ->
     let x = Registers.register_to_int vx in
     for k = 0 to x do
       let rs = Registers.register_of_int k in
       let m = Memory.read_uint8 t.memory (Uint16.(add t.i @@ of_int k)) in
       Registers.set t.registers rs m
     done;
     NEXT

let tick t =
  let opcode = Memory.read_uint16 t.memory t.pc in
  let i = Instructions.instruction_of_uint16 opcode in
  
  begin 
    match execute_instruction t i with
    | NEXT     -> t.pc <- Uint16.(add t.pc @@ of_int 2)
    | SKIP     -> t.pc <- Uint16.(add t.pc @@ of_int 4)
    | JUMP nnn -> t.pc <- nnn
  end;
  if Uint8.(compare t.delay_timer zero) = 1
  then t.delay_timer <- Uint8.(sub t.delay_timer one);
  if Uint8.(compare t.sound_timer zero) = 1
  then t.sound_timer <- Uint8.(sub t.sound_timer one)
  
