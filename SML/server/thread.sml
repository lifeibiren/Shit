structure Queue:
   sig
      type 'a t

      val new: unit -> 'a t
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a option
      val num: 'a t -> int
   end =
   struct
      datatype 'a t = T of {front: 'a list ref, back: 'a list ref}

      fun new () = T {front = ref [], back = ref []}

      fun enque (T {back, ...}, x) = back := x :: !back

      fun deque (T {front, back}) =
         case !front of
            [] => (case !back of
                      [] => NONE
                    | l => let val l = rev l
                           in case l of
                              [] => raise Fail "deque"
                            | x :: l => (back := []; front := l; SOME x)
                           end)
          | x :: l => (front := l; SOME x)

      fun num (T {front, back}) = (length (!front)) + (length (!back))
   end

structure Thread:>
   sig
      type t
      val exit: unit -> unit
      val run: unit -> unit
      val spawn: (unit -> unit) -> unit
      val yield: unit -> unit
      val suspend: unit -> unit
      val ready: t -> unit
      val new: (unit -> unit) -> t
      val async_wait: (t -> 'a option) -> 'a
   end =
   struct
      type t = {thread: MLton.Thread.Runnable.t ref,
                inQueue: bool ref,
                exited: bool ref} ref
      val topLevel: t option ref = ref NONE
      val current: t option ref = ref NONE

      local
         val threads: t Queue.t = Queue.new ()
      in
         fun ready (t as ref {inQueue = inQueue, exited = exited, ...}: t) : unit =
            if (not (!inQueue)) andalso (not (!exited)) then (
                inQueue := true;
                Queue.enque(threads, t))
            else ()
         fun next () : t =
            case Queue.deque threads of
               NONE => valOf (!topLevel)
             | SOME (t as ref {inQueue = inQueue, exited = exited, ...}) =>
                if !exited then next()
                else (inQueue := false; t)
      end
      fun saveContext newContext = case !current of
                          SOME (t as ref {thread = thread, ...}) => thread := newContext
                        | NONE => ()

      fun switch f =
            case !current of
              SOME t => MLton.Thread.switch (fn lastThread =>
                        let
                            val _ = (#thread (!t)) := MLton.Thread.prepare (lastThread, ())
                            val next as ref ({thread = this, ...}) = f t
                            val _ = current := SOME next
                        in
                            !this
                        end)

            | NONE   => MLton.Thread.switch (fn t =>
                        let
                            val main = ref {thread = ref (MLton.Thread.prepare (t, ())),
                                            inQueue = ref false,
                                            exited = ref false}
                            val _ = current := SOME main
                            val next as (ref {thread = nextThread, ...}) = f main
                            val _ = current := SOME next
                        in
                            !nextThread
                        end)

      fun exit () = (print "exited\n"; switch (fn _ => next ()))

      fun new (f: unit -> unit): t =
         ref {thread = ref (MLton.Thread.prepare (
                    MLton.Thread.new (fn () => (f (); exit ())),
                    ())),
              inQueue = ref false,
              exited = ref false}


      fun prepare (ref {thread = thisThread, inQueue = inQueue, exited = exited},  v): t =
        ref {thread = ref (MLton.Thread.prepare (thisThread ,v)), inQueue = inQueue, exited = exited}


      fun schedule (SOME t)  = (ready t; next ())
        | schedule NONE      = next()



      fun suspend (): unit = switch (fn t => schedule NONE)
      fun yield (): unit = switch (fn t => schedule (SOME t))

      val spawn = ready o new

      fun async_wait (complete: t -> 'a option) : 'a =
          case !current of
            NONE => raise Fail "current is NONE, can't do async_wait"
          | SOME th => case complete th of
                         SOME ret => ret
                       | NONE => (suspend(); async_wait complete)


      fun run(): unit =
          (switch (fn t =>
                  (topLevel := SOME t
                   ; next()))
          ; topLevel := NONE)
   end
