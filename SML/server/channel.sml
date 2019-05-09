signature CHAN =
sig
  type 'a t

  val new: int -> 'a t
  val put: 'a t -> 'a -> unit
  val get: 'a t -> 'a
  val peek: 'a t -> 'a option
  (* val tryGet: 'a t -> 'a option *)
end

structure Channel :> CHAN =
struct
  datatype 'a t = Ch of {
    reader: Thread.t list ref,
    writer: Thread.t list ref,
    queue: 'a list ref,
    capicity: int
  }

  fun new capicity = Ch {
    reader = ref nil,
    writer = ref nil,
    queue = ref nil,
    capicity = capicity
  }

  fun wakeOne (t as ref (x::xs)) = (Thread.ready x; t := xs)
    | wakeOne  _  = ()

  fun put (Ch {reader = rd, writer = wr, queue = q, capicity = cap, ...}) elem: unit =
    Thread.async_wait (
      fn th =>  if length (!q) < cap
                then (q := ((!q) @ [elem]);
                      wakeOne rd;
                      SOME ())
                else (wr := ((!wr) @ [th]);
                      NONE)
    )

  fun get (Ch {reader = rd, writer = wr, queue = q, ...}): 'a =
    Thread.async_wait (
      fn th =>  if length (!q) > 0
                then (q := List.drop (!q, 1);
                      wakeOne wr;
                      SOME (hd (!q)))
                else (rd := (!rd) @ [th];
                      NONE)
    )
  fun peek (Ch {queue = ref (q as x :: xs), ...}): 'a option = SOME x
    | peek (Ch {queue = ref (q as nil), ...}): 'a option = NONE
end
