open Chip8ml
open Stdint
open Tsdl

let keymap =
  [
    Sdl.K.k1, 0x01;
    Sdl.K.k2, 0x02;
    Sdl.K.k3, 0x03;
    Sdl.K.k4, 0x0C;
    Sdl.K.q, 0x04;
    Sdl.K.w, 0x05;
    Sdl.K.e, 0x06;
    Sdl.K.r, 0x0D;
    Sdl.K.a, 0x07;
    Sdl.K.s, 0x08;
    Sdl.K.d, 0x09;
    Sdl.K.f, 0x0E;
    Sdl.K.z, 0x0A;
    Sdl.K.x, 0x00;
    Sdl.K.c, 0x0B;
    Sdl.K.v, 0x0F;
  ] |> List.to_seq |> Hashtbl.of_seq

let load_rom (filepath : string) (memory : Memory.t) =
  let open Unix in
  let fd = openfile filepath [O_RDONLY] 0o644 in
  let rec loop addr =
    let buffer = Bytes.create 2 in
    match read fd buffer 0 2 with
    | 0 -> ()
    | _ ->
       let byte1 = Bytes.get buffer 0 |> Char.code |> Uint8.of_int in
       let byte2 = Bytes.get buffer 1 |> Char.code |> Uint8.of_int in
       Memory.write_uint8 memory (Uint16.of_int addr) byte1;
       Memory.write_uint8 memory (Uint16.of_int (addr + 1)) byte2;       
       loop (addr + 2)
  in
  loop 0x200;
  close fd

let start_game rom =
  begin
    let (let*) = Result.bind in
    let return x = Result.Ok x in
    let* _ = Sdl.init Sdl.Init.(audio + events + video) in
    let* w = Sdl.create_window "CHIP-8"
               ~x:Sdl.Window.pos_centered
               ~y:Sdl.Window.pos_centered
               ~w:640
               ~h:320
               Sdl.Window.shown in
    let* r = Sdl.create_renderer w
               ~index:(-1)
               ~flags:Sdl.Renderer.software in

    let chip8 = Cpu.create () in
    load_rom rom chip8.memory;
 
    let running_ref = ref true in
    let target_delay = Int32.of_int 2 in (* Processor is 500hz according to Reddit => 2 ms cycle *)
    let dt = ref (Int32.of_int 0) in
    let start_t = ref (Int32.of_int 0) in
    let end_t = ref (Int32.of_int 0) in
    let event = Sdl.Event.create () in
    
    while !running_ref do
      start_t := Sdl.get_ticks ();
      ignore @@ Sdl.set_render_draw_color r 47 79 79 0;
      ignore @@ Sdl.render_clear r;
      ignore @@ Sdl.set_render_draw_color r 0 255 255 0;
    
      for x = 0 to 63 do
        for y = 0 to 31 do
          if Display.is_set chip8.display_buffer x y then
            let rx = x * 10 in
            let ry = y * 10 in
            let rw = 10 in
            let rh = 10 in
            let rect = Sdl.Rect.create ~x:rx ~y:ry ~w:rw ~h:rh in
            ignore @@ Sdl.render_fill_rect r (Some rect)
          else ();
        done;
      done;
      
      Sdl.render_present r;
      Cpu.tick chip8;

      while Sdl.poll_event (Some event) do
        match Sdl.Event.(get event typ |> enum) with
        | `Quit -> running_ref := false
        | `Key_down -> begin
            let key = Sdl.Event.(get event keyboard_keycode) in
            match Hashtbl.find_opt keymap key with
            | Some k -> Keyboard.key_down chip8.keyboard_buffer k;
            | None -> ()
          end
        | `Key_up -> begin
            let key = Sdl.Event.(get event keyboard_keycode) in
            match Hashtbl.find_opt keymap key with
            | Some k -> Keyboard.key_up chip8.keyboard_buffer k;
            | None -> ()
          end
        | _ -> ()
      done;

      end_t := Sdl.get_ticks ();
      dt := Int32.sub !end_t !start_t;

      if target_delay > !dt then
        Sdl.delay @@ Int32.sub target_delay !dt;
    done;
    
    Sdl.destroy_renderer r;
    Sdl.destroy_window w;
    Sdl.quit ();
    return ()
  end


let main () =           
  Sys.argv.(1) |> start_game
  
let () =
  match main () with
  | Ok _ -> ()
  | _ -> ()
