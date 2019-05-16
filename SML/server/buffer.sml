signature Buffer =
sig
  type 'a t
  val buffer: int * 'a -> 'a t
  val array: 'a t -> a' array
  val vector: 'a t -> 'a vector
  val length: 'a t -> int
  val sub: 'a t * int -> 'a
  val update: 'a t * int * 'a -> unit
  val concat: 'a t * 'a t -> 'a t
end
