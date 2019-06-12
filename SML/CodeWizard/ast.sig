signature AST =
sig
  structure Node:
  sig
    type 'a t
    val node: 'a -> 'a t list -> 'a t
    val child: 'a t -> 'a t list
  end

  type 'a t
  val tree: 'a Node.t -> 'a t
  val root: 'a t -> 'a Node.t
  (* val tranverse: 'a t -> (a' Node.t -> 'b) -> 'a t *)
end
