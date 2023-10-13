module Platform_map : (Map.S with type key = string)

module Switch_map : (Map.S with type key = Ocaml_version.t)

type state = Ok | Failed | Active

type t = state Switch_map.t Platform_map.t * state Platform_map.t

val update : platform:string -> switch:Ocaml_version.t option -> state -> unit

val get : unit -> t
