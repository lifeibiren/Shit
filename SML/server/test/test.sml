val count = ref 0
val thread1 = ref NONE
val thread2 = ref NONE

fun readyRef tRef : unit =
    case !tRef of
      SOME t => Thread.ready t
    | NONE => ()

fun threadRoutine1 tRef : unit =
    let
        val _ = print ("Routine 1\n")
        val _ = count := !count + 1
        val _ = readyRef tRef
        val _ = Thread.yield ()
        val _ = print ("Back to Routine 1\n")
        val _ = count := !count + 1
        val _ = readyRef tRef
        val _ = Thread.exit ()
    in
        ()
    end

fun threadRoutine2 tRef : unit =
    let
        val _ = print ("Routine 2\n")
        val _ = count := !count + 1
        val _ = readyRef tRef
        val _ = Thread.yield ()
        val _ = print ("Back to Routine 2\n")
        val _ = count := !count + 1
        val _ = Thread.exit ()
    in
        ()
    end

val _ = thread1 := SOME (Thread.new (fn () => threadRoutine1 thread2))
val _ = thread2 := SOME (Thread.new (fn () => threadRoutine2 thread1))
val _ = readyRef thread1
val _ = Thread.run ()
val _ = print("Final Count " ^ (Int.toString (!count)) ^ "\n")
val _ = print("Return to Main\n")
