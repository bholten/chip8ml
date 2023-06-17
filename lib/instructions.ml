open Stdint

type t =
  | CLS
  | RET
  | JP_nnn of uint16
  | CALL_nnn of uint16
  | SE_Vx_nn of Registers.register * uint8
  | SNE_Vx_nn of Registers.register * uint8
  | SE_Vx_Vy of Registers.register * Registers.register
  | LD_Vx_nn of Registers.register * uint8
  | ADD_Vx_nn of Registers.register * uint8
  | LD_Vx_Vy of Registers.register * Registers.register
  | OR_Vx_Vy of Registers.register * Registers.register
  | AND_Vx_Vy of Registers.register * Registers.register
  | XOR_Vx_Vy of Registers.register * Registers.register
  | ADD_Vx_Vy_Vf of Registers.register * Registers.register
  | SUB_Vx_Vy_Vf of Registers.register * Registers.register
  | SHR_Vy of Registers.register * Registers.register
  | SUBN_Vx_Vy_Vf of Registers.register * Registers.register
  | SHL_Vx_Vy of Registers.register * Registers.register
  | SNE_Vx_Vy of Registers.register * Registers.register
  | LD_I of uint16
  | JP_V0 of uint16
  | RND_Vx_nn of Registers.register * uint8
  | DRW_Vx_Vy_n of Registers.register * Registers.register * uint8
  | SKP_Vx of Registers.register
  | SKNP_Vx of Registers.register
  | LD_Vx_Dt of Registers.register
  | LD_Vx_n of Registers.register * uint8
  | LD_Dt_Vx of Registers.register
  | LD_St_Vx of Registers.register
  | ADD_I_Vx of Registers.register
  | LD_F_Vx of Registers.register
  | LD_B_Vx of Registers.register
  | LD_I_Vx of Registers.register
  | LD_Vx_I of Registers.register

let instruction_of_uint16 (n : uint16) =
  let opcode = Uint16.to_int n in
  match opcode with
  | 0x00E0 -> CLS
  | 0x00EE -> RET
  | _ ->
     let x = (opcode land 0x0F00) lsr 8 in
     let y = (opcode land 0x00F0) lsr 4 in
     let vx = Registers.register_of_int x in
     let vy = Registers.register_of_int y in
     let n' = opcode land 0x000F in
     let nn' = opcode land 0x00FF in
     let n = Uint8.of_int n' in
     let nn = Uint8.of_int nn' in
     let nnn = Uint16.of_int @@ opcode land 0x0FFF in
     match opcode lsr 12 with
     | 0x1 -> JP_nnn nnn
     | 0x2 -> CALL_nnn nnn
     | 0x3 -> SE_Vx_nn (vx, nn)
     | 0x4 -> SNE_Vx_nn (vx, nn)
     | 0x5 -> SE_Vx_Vy (vx, vy)
     | 0x6 -> LD_Vx_nn (vx, nn)
     | 0x7 -> ADD_Vx_nn (vx, nn)
     | 0x8 ->
        begin match n' with
        | 0x0 -> LD_Vx_Vy (vx, vy)
        | 0x1 -> OR_Vx_Vy (vx, vy)
        | 0x2 -> AND_Vx_Vy (vx, vy)
        | 0x3 -> XOR_Vx_Vy (vx, vy)
        | 0x4 -> ADD_Vx_Vy_Vf (vx, vy)
        | 0x5 -> SUB_Vx_Vy_Vf (vx, vy)
        | 0x6 -> SHR_Vy (vx, vy)
        | 0x7 -> SUBN_Vx_Vy_Vf (vx, vy)
        | 0xE -> SHL_Vx_Vy (vx, vy)
        | _ -> failwith "Invalid opcode 8nnn"
        end
     | 0x9 ->
        begin match n' with
        | 0x0 -> SNE_Vx_Vy (vx, vy)
        | _ -> failwith "Invalid opcode 9nnn"
        end
     | 0xA -> LD_I nnn
     | 0xB -> JP_V0 nnn
     | 0xC -> RND_Vx_nn (vx, nn)
     | 0xD -> DRW_Vx_Vy_n (vx, vy, n)
     | 0xE ->
        begin match nn' with
        | 0x9E -> SKP_Vx vx
        | 0xA1 -> SKNP_Vx vx
        | _ -> failwith "Invalid opcode Ennn"
        end
     | 0xF ->
        begin match nn' with
        | 0x07 -> LD_Vx_Dt vx
        | 0x0A -> LD_Vx_n (vx, n)
        | 0x15 -> LD_Dt_Vx vx
        | 0x18 -> LD_St_Vx vx
        | 0x1E -> ADD_I_Vx vx
        | 0x29 -> LD_F_Vx vx
        | 0x33 -> LD_B_Vx vx
        | 0x55 -> LD_I_Vx vx
        | 0x65 -> LD_Vx_I vx
        | _ -> failwith "Invalid opcode Fnnn"
        end
     | _ -> failwith "Invalid opcode"
     
