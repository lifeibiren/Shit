val count = ref 0
val thread1 = ref NONE
val thread2 = ref NONE
val thread3 = ref NONE
fun readyRef tRef : unit =
    case !tRef of
      SOME t => Thread.ready t
    | NONE => ()

fun threadRoutine1 tRef : unit =
    let
        val _ = print ("Routine 1\n")
        val _ = count := !count + 1
        val _ = readyRef thread3
        val _ = Thread.suspend ()
        val _ = readyRef tRef
        val _ = Thread.suspend ()
        val _ = print ("Back to Routine 1\n")
        val _ = count := !count + 1
        val _ = readyRef tRef
    in
        ()
    end

fun threadRoutine2 tRef : unit =
    let
        val _ = print ("Routine 2\n")
        val _ = count := !count + 1
        val _ = readyRef tRef
        val _ = Thread.suspend ()
        val _ = print ("Back to Routine 2\n")
        val _ = count := !count + 1
    in
        ()
    end

fun threadRoutine3 () : unit =
    let
        val _ = print("Routine 3 Waits while Count < 1\n")
        val _ = readyRef thread1
        val _ = print(Thread.async_wait (
                        fn () => if !count < 1
                             then NONE
                             else SOME "Routine Complete\n"))
        val _ = readyRef thread1
    in () end


val _ = thread1 := SOME (Thread.new (fn () => threadRoutine1 thread2))
val _ = thread2 := SOME (Thread.new (fn () => threadRoutine2 thread1))
val _ = thread3 := SOME (Thread.new threadRoutine3)
val _ = readyRef thread3
val _ = Thread.run ()
val _ = print("Final Count " ^ (Int.toString (!count)) ^ "\n")
val _ = print("Return to Main\n")
