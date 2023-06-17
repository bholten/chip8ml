module type Video = sig
  type t

  val init : t -> Cpu.t -> unit
  val draw : t -> Cpu.t -> unit
end

module type Input = sig
  type t

  val keymap : int -> Cpu.t -> int
  val key_state : t -> Cpu.t -> bool
end
