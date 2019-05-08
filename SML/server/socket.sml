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
    val setReader: t * (t -> unit) option -> unit
    val setWriter: t * (t -> unit) option -> unit
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
    type ('af, 'sock_type) t
    structure Completion:
    sig
      type t
      val any: t
      val all: t
      val atLeast: int -> t
    end
    val socket: ('af, 'sock_type) Socket.sock * EventLoop.t ->
                  ('af, 'sock_type) t
    val accept: ('af, Socket.passive Socket.stream) t ->
                  (('af, Socket.active Socket.stream) Socket.sock *
                  'af Socket.sock_addr)
    val recvArrGen: ('af, Socket.active Socket.stream) t *
                      Word8ArraySlice.slice *
                      Completion.t -> int
    val sendArrGen: ('af, Socket.active Socket.stream) t *
                      Word8ArraySlice.slice *
                      Completion.t -> int
    val recvArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val sendArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val close: ('af, 'sock_type) t -> unit
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
    fun setReader (T (ref {reader = reader, ...}), r): unit = reader := r
    fun setWriter (T (ref {writer = writer, ...}), w): unit = writer := w
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
            val _ = Thread.run ()
            val pollDesc = makePollDesc l
            val pollInfo = poll (pollDesc, time)
            fun callHandler info =
              let
                fun callHandler get e: unit =
                  case get e of
                    SOME f => f e
                  | NONE   => ()
                val desc = (pollToIODesc o infoToPollDesc) info
                val _ = case getEvent l desc of
                              (* NONE => raise Bug "Event not found" *)
                          NONE => ()
                        | SOME e => (if isIn info
                                     then callHandler Event.getReader e
                                     else ();
                                     if isOut info
                                     then callHandler Event.getWriter e
                                     else ())
              in
                ()
              end
            val _ = map (fn info => callHandler info) pollInfo
            val _ = Thread.run ()
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

    type 'a final = 'a MLton.Finalizable.t

    datatype ('af, 'sock_type) st = T of
      {socket: ('af, 'sock_type) Socket.sock,
       event: Event.t,
       eventLoop: EventLoop.t,
       closed: bool ref}

    type ('af, 'sock_type) t = ('af, 'sock_type) st final

    fun setupReader thread event: unit =
      Event.setReader (event, SOME (fn _ => (Event.setReader (event, NONE);
                                             Thread.ready thread)))
    fun setupWriter thread event: unit =
      Event.setWriter (event, SOME (fn _ => (Event.setWriter (event, NONE);
                                            Thread.ready thread)))

    fun doClose (T {socket = sock, event = e, eventLoop = lp, closed = c, ...}) =
      if !c then ()
      else (c := true; EventLoop.delEvent (lp, e);
            Socket.close sock)

    fun close s: unit =
        MLton.Finalizable.withValue (s, fn s => doClose s)

    fun socket (sock, loop): ('af, 'sock_type) t =
      let val e = Event.event (Socket.ioDesc sock, NONE, NONE)
          val _ = EventLoop.addEvent (loop, e)
          (* Setup finalizer to close socket when it's no longer available *)
          val s = T {socket = sock, event = e, eventLoop = loop, closed = ref false}
          val ret =  MLton.Finalizable.new s
          val _ = MLton.Finalizable.addFinalizer (ret, fn s => doClose s)
      in ret
      end

    fun accept s =
        MLton.Finalizable.withValue (s,
          fn T {socket = sock, event = e, ...} =>
             Thread.async_wait (
              fn th => case Socket.acceptNB sock of
                         NONE   => (setupReader th e; NONE)
                       | SOME (newSock, addr) => SOME (newSock, addr)
            )
          )


    structure Completion =
    struct
      type t = int -> int -> bool
      fun any maxLen len = if len > 0 then true
                           else false
      fun atLeast exp maxLen len = if len >= exp orelse len = maxLen then true
                                   else false
      fun all maxLen len = if len = maxLen then true
                           else false
    end

    fun recvArrGen (s, arr, completion) =
      MLton.Finalizable.withValue (s,
        fn T {socket = sock, event = e, ...} =>
          let val len = ref 0
              val error = ref false
              val maxLen = Word8ArraySlice.length arr
              fun loop th =
                let val subArr = Word8ArraySlice.subslice (arr, !len, NONE)
                    val _ =  case Socket.recvArrNB (sock, subArr) of
                               NONE => ()
                             | SOME n => case n of
                                           0 => error := true
                                         | _ => len := !len + n
                in if !error orelse completion maxLen (!len)
                   then SOME (!len)
                   else (setupReader th e; NONE)
                end
          in  Thread.async_wait loop
          end
      )

    fun sendArrGen (s, arr, completion) =
      MLton.Finalizable.withValue (s,
        fn T {socket = sock, event = e, ...} =>
          let val len = ref 0
              val error = ref false
              val maxLen = Word8ArraySlice.length arr
              fun loop th =
                let val subArr = Word8ArraySlice.subslice (arr, !len, NONE)
                    val _ =  case Socket.sendArrNB (sock, subArr) of
                               NONE => ()
                             | SOME n => case n of
                                           0 => error := true
                                         | _ => len := !len + n
                in if !error orelse completion maxLen (!len)
                   then SOME (!len)
                   else (setupWriter th e; NONE)
                end
          in  Thread.async_wait loop
          end
      )

      fun recvArr (sock, arr) = recvArrGen (sock, arr, Completion.all)
      fun recvSomeArr (sock, arr) = recvArrGen (sock, arr, Completion.any)
      fun sendArr (sock, arr) = sendArrGen (sock, arr, Completion.all)
      fun sendSomeArr (sock, arr) = sendArrGen (sock, arr, Completion.any)
  end
end

val sockfd = INetSock.TCP.socket ()
val _ = Socket.bind (sockfd, INetSock.any 1200)
val _ = Socket.listen (sockfd, 5)
val desc = Socket.ioDesc sockfd
val lp = Asio.EventLoop.eventLoop ()
val aSock = Asio.Socket.socket (sockfd, lp)
val _ = MLton.Signal.setHandler (Posix.Signal.pipe, MLton.Signal.Handler.ignore)

fun prepareZeroArr len = (Word8ArraySlice.full o Word8Array.array) (len, Word8.fromInt 0)
fun recvAsArr (sock, len) =
  let val arr = prepareZeroArr len
      val _ = Asio.Socket.recvArr (sock, arr)
  in arr
  end
fun arrToIPv4 addr = ""

fun recvClientHello sock: Word8.word * Word8.word * Word8ArraySlice.slice =
  let val arr = recvAsArr (sock, 2)
      val ver = Word8ArraySlice.sub (arr, 0)
      val nmethods = Word8ArraySlice.sub (arr, 1)
      val methods = recvAsArr (sock, Word8.toInt nmethods)
  in (ver, nmethods, methods)
  end

fun sendServerHello sock (ver: Word8.word, method: Word8.word): unit =
    (Asio.Socket.sendArr (sock, (Word8ArraySlice.full o Word8Array.fromList) [ver, method]); ())

datatype CmdType = CONNECT | BIND | UDP
fun cmdTypeFromInt 1 = CONNECT
  | cmdTypeFromInt 2 = BIND
  | cmdTypeFromInt 3 = UDP
  | cmdTypeFromInt _ = raise Fail "Invalid cmd type"

fun CmdTypeToString CONNECT = "connect"
  | CmdTypeToString BIND    = "bind"
  | CmdTypeToString UDP     = "udp"

fun recvClientReq sock: CmdType * string * int =
  let val arr = recvAsArr (sock, 4)
      val ver = Word8ArraySlice.sub (arr, 0)
      val cmd = (cmdTypeFromInt o Word8.toInt o Word8ArraySlice.sub) (arr, 1)
      val atype = (Word8.toInt o Word8ArraySlice.sub) (arr, 3)
      val addr = case atype of
                1 => (Byte.unpackString o recvAsArr) (sock, 4)
              | 3 => let val arr = recvAsArr (sock, 1)
                         val host = recvAsArr (sock, Word8.toInt (Word8ArraySlice.sub (arr, 0)))
                      in Byte.unpackString host
                      end
              | 4 => ""
              | _ => raise Fail "Invalid address type"
      val port = let val arr = recvAsArr (sock, 2)
                     open LargeWord
                     infix << andb
                 in toInt ((((Word8.toLarge o Word8ArraySlice.sub) (arr, 0)) << 0w8) andb
                    ((Word8.toLarge o Word8ArraySlice.sub) (arr, 1)))
                 end
  in (cmd, addr, port)
  end

fun socks5Thread sock =
  let val (ver, nmethods, methods) = recvClientHello sock
      val _ = print ("Socks Version " ^ (Word8.toString ver) ^ "\n" )
      val _ = sendServerHello sock (0w5, 0w0)
      val (cmd, addr, port) = recvClientReq sock
      val _ = print ((CmdTypeToString cmd) ^ " " ^ addr ^ " : " ^ (Int.toString port) ^ "\n" )
  in
    ()
  end

fun connectionThread sock =
  let val arr = Word8Array.array (10, Word8.fromInt 0)
      val slice = Word8ArraySlice.full arr
      fun work () =
        let val len = Asio.Socket.recvArr (sock, slice)
            val _ = print ("Received " ^ (Int.toString len) ^ " bytes from client\n")
            val toSend = Word8ArraySlice.slice (arr, 0, SOME len)
            val _ = Asio.Socket.sendArr (sock, toSend)
            val _ = print ("Sent " ^ (Int.toString len) ^ " bytes to cleint\n")
        in if len <> 0 then work ()
           else ()
        end
  in (work (); MLton.GC.collect ())
  end
fun acceptThread () =
  let val (newSock, addr) = Asio.Socket.accept aSock
      val (inAddr, port) = INetSock.fromAddr addr
      val _ = print ("New Connection " ^ (NetHostDB.toString inAddr) ^
                     " : " ^ (Int.toString port) ^ " " ^ "\n")
      val _ = Thread.spawn (fn () => (socks5Thread o Asio.Socket.socket)
                                      (newSock, lp))
  in acceptThread () end

val _ = Thread.spawn acceptThread
val _ = Asio.EventLoop.run lp
