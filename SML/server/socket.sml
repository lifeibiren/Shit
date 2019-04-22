signature ASIO =
sig
  type iodesc = OS.IO.iodesc

  structure Event:
  sig
    type t
    val event: iodesc * (t -> unit) option * (t -> unit) option -> t
    val getReader: t -> (t -> unit) option
    val getWriter: t -> (t -> unit) option
    val getIodesc: t -> iodesc
    val setReader: t -> (t -> unit) -> unit
    val setWriter: t -> (t -> unit) -> unit
  end

  (* structure Timer:
  sig
    val timer: unit -> t
    val setTimeOut: t * int * (t -> unit) -> unit
    val cancel: t -> unit
    val destroy: t -> unit
  end *)

  structure EventLoop:
  sig
    type t
    val eventLoop: unit -> t
    val addEvent: t * Event.t -> unit
    val delEvent: t * Event.t -> unit
    val runOnce: t -> int
    val run: t -> unit
    (* val stop: t -> unit *)
  end

  structure Socket:
  sig
    val accept: ('af, Socket.passive Socket.stream) Socket.sock
                  -> (('af, Socket.active Socket.stream) Socket.sock *
                      'af Socket.sock_addr) option
    val recvArr: ('af, Socket.active Socket.stream) Socket.sock *
                   Word8ArraySlice.slice -> int option
    val sendArr: ('af, Socket.active Socket.stream) Socket.sock *
                   Word8ArraySlice.slice -> int option
  end
end

structure Asio :> ASIO =
struct
  exception Bug of string
  type iodesc = OS.IO.iodesc

  structure Event=
  struct
    datatype t = T of {desc: iodesc,
                       reader: (t -> unit) option ref,
                       writer: (t -> unit) option ref} ref
    fun event (iodesc, reader, writer): t =
        T (ref {desc = iodesc, reader = ref reader, writer = ref writer})
    fun getReader (T (ref {reader = reader, ...})): (t -> unit) option = !reader
    fun getWriter (T (ref {writer = writer, ...})): (t -> unit) option = !writer
    fun getIodesc (T (ref {desc = desc, ...})): iodesc = desc
    fun setReader (T (ref {reader = reader, ...})) r: unit = reader := r
    fun setWriter (T (ref {writer = writer, ...})) w: unit = writer := w
  end

  (* structure Timer =
  struct
    val timer: unit -> t
    val setTimeOut: t * int * (t -> unit) -> unit
    val cancel: t -> unit
    val destroy: t -> unit
  end *)

  structure EventLoop =
  struct
    type t = Event.t list ref
    fun eventLoop (): t = ref nil
    fun addEvent (l, e): unit = l := e :: (!l)
    fun delEvent (l, e): unit = l := List.filter (fn x =>
          OS.IO.compare (Event.getIodesc x, Event.getIodesc e) <> EQUAL) (!l)
    fun makePollDesc l: OS.IO.poll_desc list =
          List.mapPartial (fn x =>
            let
              fun checkReader desc = case Event.getReader x of
                                       SOME _ => OS.IO.pollIn desc
                                     | NONE => desc
              fun checkWriter desc = case Event.getWriter x of
                                       SOME _ => OS.IO.pollOut desc
                                     | NONE => desc
            in
              case (OS.IO.pollDesc o Event.getIodesc) x of
                SOME desc => SOME ((checkReader o checkWriter) desc)
              | NONE => NONE
            end) (!l)

    fun getEvent l ioDesc: Event.t option =
          List.find (fn x => OS.IO.compare (Event.getIodesc x, ioDesc) = EQUAL) (!l)

    fun doPoll l time: int =
          let
            open OS.IO
            val pollDesc = makePollDesc l
            val pollInfo = poll (pollDesc, time)
            fun callHandler info =
              let
                val desc = (pollToIODesc o infoToPollDesc) info
                val event = case getEvent l desc of
                              SOME e => e
                            | NONE => raise Bug "Event not found"
                val _ = if isIn info then case Event.getReader event of
                                            SOME f => f event
                                          | NONE => ()
                        else ()
                val _ = if isOut info then case Event.getWriter event of
                                             SOME f => f event
                                           | NONE => ()
                        else ()
              in
                ()
              end
            val _ = map (fn info => callHandler info) pollInfo
          in
            length pollInfo
          end

    fun runOnce l: int = doPoll l (SOME Time.zeroTime)
    fun run l: unit =
          let fun doRun () = (doPoll l NONE; doRun ())
          in doRun () end
  end

  structure Socket =
  struct

    fun accept sock = Thread.async_wait (
        fn () => case Socket.acceptNB sock of
                   NONE   => NONE
                 | SOME (newSock, addr) => SOME (SOME (newSock, addr))
      )

    fun recvArr (sock, arr) =
      let val len = ref 0
          val maxLen = Word8ArraySlice.length arr
          fun loop () =
            let val subArr = Word8ArraySlice.subslice (arr, !len, NONE)
            in  case Socket.recvArrNB (sock, subArr) of
                  NONE   => NONE
                | SOME n => (len := !len + n;
                             if n = 0 orelse !len = maxLen
                             then SOME (SOME (!len))
                             else NONE)
            end
      in  Thread.async_wait loop
      end

    fun sendArr (sock, arr) =
      let val len = ref 0
          val maxLen = Word8ArraySlice.length arr
          fun loop () =
            let val subArr = Word8ArraySlice.subslice (arr, !len, NONE)
            in  case Socket.sendArrNB (sock, subArr) of
                  NONE   => NONE
                | SOME n => (len := !len + n;
                             if n = 0 orelse !len = maxLen
                             then SOME (SOME (!len))
                             else NONE)
            end
      in  Thread.async_wait loop
      end
  end
end

(* fun main () = *)
(* let *)


val sockfd = INetSock.TCP.socket ()
val _ = Socket.bind (sockfd, INetSock.any 1400)
val _ = Socket.listen (sockfd, 5)
val desc = Socket.ioDesc sockfd
val lp = Asio.EventLoop.eventLoop()

fun handler e = case Asio.Socket.accept sockfd of
            NONE => ()
          | SOME (newSock, addr) =>
              let val (inAddr, port) = INetSock.fromAddr addr
                  val _ = print ("New Connection " ^ (NetHostDB.toString inAddr) ^
                                 " : " ^ (Int.toString port) ^ " " ^ "\n")
              in () end


val _ = Asio.EventLoop.addEvent (lp, Asio.Event.event (desc, SOME handler, NONE))
val _ = Asio.EventLoop.run lp
(* in () end *)
(* val _ = Thread.spawn (fn () => main()) *)
(* val _ = Thread.run() *)
