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
    val connect: ('af, 'sock_type) t * 'af Socket.sock_addr -> unit

    val sendVecGen: ('af, Socket.active Socket.stream) t *
                      Word8VectorSlice.slice *
                      Completion.t -> int

    val recvArrGen: ('af, Socket.active Socket.stream) t *
                      Word8ArraySlice.slice *
                      Completion.t -> int

    val sendArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val sendVec: ('af, Socket.active Socket.stream) t * Word8VectorSlice.slice -> int
    val sendSomeArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val sendSomeVec: ('af, Socket.active Socket.stream) t * Word8VectorSlice.slice -> int

    val recvArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val recvVec: ('af, Socket.active Socket.stream) t * int -> Word8VectorSlice.slice
    val recvSomeArr: ('af, Socket.active Socket.stream) t * Word8ArraySlice.slice -> int
    val recvSomeVec: ('af, Socket.active Socket.stream) t * int -> Word8VectorSlice.slice

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
                fun doCallHandler get e: unit =
                  case get e of
                    SOME f => f e
                  | NONE   => ()
                val desc = (pollToIODesc o infoToPollDesc) info
                val _ = case getEvent l desc of
                              (* NONE => raise Bug "Event not found" *)
                          NONE => ()
                        | SOME e => (if isIn info
                                     then doCallHandler Event.getReader e
                                     else ();
                                     if isOut info
                                     then doCallHandler Event.getWriter e
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
    exception SocketClosed;

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

    fun connect (s, addr) =
      MLton.Finalizable.withValue (s,
        fn T {socket = sock, event = e, ...} =>
          case Socket.connectNB (sock, addr) of
            true => ()
          | false =>
              let val setup = ref true
              in  Thread.async_wait (
                    fn th =>  case !setup of
                                true => (
                                  setup := false;
                                  setupWriter th e;
                                  NONE
                                )
                              | false => SOME ()
                  )
              end
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

    fun sendVecGen (s, arr, completion) =
      MLton.Finalizable.withValue (s,
        fn T {socket = sock, event = e, ...} =>
          let val len = ref 0
              val error = ref false
              val maxLen = Word8VectorSlice.length arr
              fun loop th =
                let val subArr = Word8VectorSlice.subslice (arr, !len, NONE)
                    val _ =  case Socket.sendVecNB (sock, subArr) of
                               NONE => ()
                             | SOME n => case n of
                                           0 => (error := true; raise SocketClosed)
                                         | _ => len := !len + n
                in if !error orelse completion maxLen (!len)
                   then SOME (!len)
                   else (setupWriter th e; NONE)
                end
          in  Thread.async_wait loop
          end
      )

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
                                             0 => (error := true; raise SocketClosed)
                                           | _ => len := !len + n
                  in if !error orelse completion maxLen (!len)
                     then SOME (!len)
                     else (setupReader th e; NONE)
                  end
            in  Thread.async_wait loop
            end
        )
      fun recvArr (sock, arr) = recvArrGen (sock, arr, Completion.all)
      fun recvSomeArr (sock, arr) = recvArrGen (sock, arr, Completion.any)
      fun recvVec (sock, len) =
        let val arr = Word8Array.array (len, Word8.fromInt 0)
            val len = recvArr (sock, Word8ArraySlice.full arr)
            val vec = Word8Array.vector arr
        in
          Word8VectorSlice.slice (vec, 0, SOME(len))
        end
      fun recvSomeVec (sock, len) =
        let val arr = Word8Array.array (len, Word8.fromInt 0)
            val len = recvSomeArr (sock, Word8ArraySlice.full arr)
            val vec = Word8Array.vector arr
        in
          Word8VectorSlice.slice (vec, 0, SOME(len))
        end

      fun sendVec (sock, vec) = sendVecGen (sock, vec, Completion.all)
      fun sendSomeVec (sock, vec) = sendVecGen (sock, vec, Completion.any)
      fun sendArr (sock, arr) =
        sendVec (sock, (Word8VectorSlice.full o Word8ArraySlice.vector) arr)
      fun sendSomeArr (sock, arr) =
        sendSomeVec (sock, (Word8VectorSlice.full o Word8ArraySlice.vector) arr)
  end
end

val listenSockfd = INetSock.TCP.socket ()
val _ = Socket.bind (listenSockfd, INetSock.any 1200)
val _ = Socket.listen (listenSockfd, 5)
(* val listenDesc = Socket.ioDesc listenSockfd *)
val lp = Asio.EventLoop.eventLoop ()
val listenSocket = Asio.Socket.socket (listenSockfd, lp)
val _ = MLton.Signal.setHandler (Posix.Signal.pipe, MLton.Signal.Handler.ignore)


fun vecToIPv4String vec =
  let val sub = Word8VectorSlice.sub
      val str = Int.toString o Word8.toInt
      fun conv i = (str o sub) (vec, i)
      infix sub
  in (conv 0) ^ "." ^ (conv 1) ^ "." ^ (conv 2) ^ "." ^ (conv 3)
  end
datatype CmdType = CONNECT | BIND | UDP
fun cmdTypeFromInt 1 = CONNECT
  | cmdTypeFromInt 2 = BIND
  | cmdTypeFromInt 3 = UDP
  | cmdTypeFromInt _ = raise Fail "invalid command type"

fun CmdTypeToString CONNECT = "connect"
  | CmdTypeToString BIND    = "bind"
  | CmdTypeToString UDP     = "udp"

datatype AddrType = IPV4 | DOMAINNAME | IPV6
fun addrTypeFromInt 1 = IPV4
  | addrTypeFromInt 3 = DOMAINNAME
  | addrTypeFromInt 4 = IPV6
  | addrTypeFromInt _ = raise Fail "invalid address type"


(* local *)
  val key = "01234567890123456789012345678901"
  val iv = "0123456789012345"
(* in
  val ectx = Evp.Aes.new ()
  val _ = Evp.Aes.encryptInit (ectx, Evp.Aes.Mode.ecb, Evp.Aes.Engine.default,
            Byte.stringToBytes key, Byte.stringToBytes iv)
  val dctx = Evp.Aes.new ()
  val _ = Evp.Aes.decryptInit (dctx, Evp.Aes.Mode.ecb, Evp.Aes.Engine.default,
            Byte.stringToBytes key, Byte.stringToBytes iv)
end *)

val _ = case tunnelMode of
          false => ()
        | _ => print ("Enter tunnel mode\n")

fun recvSomeDecryptVec ctx (sock, len) =
  let val cipher = Asio.Socket.recvSomeVec (sock, len)
  in Word8VectorSlice.full (Evp.Aes.decryptUpdate (ctx, Word8VectorSlice.vector cipher))
  end
fun recvDecryptVec ctx (sock, len) =
  let val cipher = Asio.Socket.recvVec (sock, len)
  in Word8VectorSlice.full (Evp.Aes.decryptUpdate (ctx, Word8VectorSlice.vector cipher))
  end
fun sendEncryptVec ctx (sock, vec) =
  let val cipher = Evp.Aes.encryptUpdate (ctx, Word8VectorSlice.vector vec)
  in Asio.Socket.sendVec (sock, Word8VectorSlice.full cipher)
  end

fun doPiping from to =
  let val vec = from ()
      val _ = to vec
  in
    if Word8VectorSlice.length vec <> 0 then doPiping from to
    else ()
  end

(* connect to remote peer and pipe data in both directions *)


fun tunnelThread sock =
  let val _ = print("tunnel thread\n")
      val ectx = Evp.Aes.new ()
      val _ = Evp.Aes.encryptInit (ectx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
              Byte.stringToBytes key, Byte.stringToBytes iv)
      val dctx = Evp.Aes.new ()
      val _ = Evp.Aes.decryptInit (dctx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
              Byte.stringToBytes key, Byte.stringToBytes iv)
      val addr = "45.63.7.148"
      val port = 1200
      val sockfd = INetSock.TCP.socket ()
      val desc = Socket.ioDesc sockfd
      val aSock = Asio.Socket.socket (sockfd, lp)
      val _ = case NetHostDB.getByName addr of
                NONE => raise Fail "Unable to resolve"
              | SOME en => (
                  print ("Connecting to " ^ ((NetHostDB.toString o NetHostDB.addr) en) ^ "\n");
                  Asio.Socket.connect (
                  aSock, INetSock.toAddr (
                      (NetHostDB.addr en), port
                  )
                ))
      val _ = print("Connected\n")
      fun fromServer () = recvSomeDecryptVec dctx (aSock, 4096)
      fun fromLocal () = Asio.Socket.recvSomeVec (sock, 4096)
      fun toServer vec = sendEncryptVec ectx (aSock, vec)
      fun toLocal  vec = Asio.Socket.sendVec (sock, vec)
      val _ = Thread.spawn (fn () => doPiping fromServer toLocal
                            handle SocketClosed => (Asio.Socket.close sock;
                                                    Asio.Socket.close aSock))
      val _ = doPiping fromLocal toServer
              handle SocketClosed => (Asio.Socket.close sock; Asio.Socket.close aSock)
      val _ = Evp.Aes.free ectx
      val _ = Evp.Aes.free dctx
  in
    ()
  end


  (* connectAndPiping sock "45.63.7.148" 1200 *)

fun ServerThread sock =
  let val ectx = Evp.Aes.new ()
      val _ = Evp.Aes.encryptInit (ectx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
              Byte.stringToBytes key, Byte.stringToBytes iv)
      val dctx = Evp.Aes.new ()
      val _ = Evp.Aes.decryptInit (dctx, Evp.Aes.Mode.cfb, Evp.Aes.Engine.default,
              Byte.stringToBytes key, Byte.stringToBytes iv)
    local
      val recvVec = recvDecryptVec dctx
      val sendVec = sendEncryptVec ectx
      val recvSomeVec = recvSomeDecryptVec dctx
    in
      fun recvClientHello sock: Word8.word * Word8.word * Word8VectorSlice.slice =
        let val vec = recvVec (sock, 2)
            val ver = Word8VectorSlice.sub (vec, 0)
            val nmethods = Word8VectorSlice.sub (vec, 1)
            val methods = recvVec (sock, Word8.toInt nmethods)
        in (ver, nmethods, methods)
        end

      fun sendServerHello sock (ver: Word8.word, method: Word8.word): unit =
          (sendVec (sock, (Word8VectorSlice.full o Word8Vector.fromList) [ver, method]); ())

      fun recvClientReq sock: CmdType * string * int =
        let val vec = recvVec (sock, 4)
            val ver = Word8VectorSlice.sub (vec, 0)
            val cmd = (cmdTypeFromInt o Word8.toInt o Word8VectorSlice.sub) (vec, 1)
            val atype = (Word8.toInt o Word8VectorSlice.sub) (vec, 3)
            val addr = case addrTypeFromInt atype of
                      IPV4 => (vecToIPv4String o recvVec) (sock, 4)
                    | DOMAINNAME =>   let val vec = recvVec (sock, 1)
                                          val hostLen = Word8.toInt (Word8VectorSlice.sub (vec, 0))
                                          val host = recvVec (sock, hostLen)
                                      in Byte.unpackStringVec host
                                      end
                    (* | IPV6 => "" *)
                    | _ => raise Fail "Invalid address type"
            val port = let val vec = recvVec (sock, 2)
                       in ((Word8.toInt o Word8VectorSlice.sub) (vec, 0)) * 256 +
                          ((Word8.toInt o Word8VectorSlice.sub) (vec, 1))
                       end
        in (cmd, addr, port)
        end

      fun sendServerRep sock newSock =
        let val localAddr = Socket.Ctl.getSockName newSock
            val (inAddr, port) = INetSock.fromAddr localAddr
            val inAddrStr = NetHostDB.toString inAddr
            val _ = print ("Local address " ^ inAddrStr ^
                           " : " ^ (Int.toString port) ^ " " ^ "\n")
            val portArr = [Word8.fromInt (port div 256), Word8.fromInt (port mod 256)]
            val arr1 = [0w5, 0w0, 0w0, 0w3, Word8.fromInt (size inAddrStr)]
            val arr2 = Word8Vector.foldl (fn (a, b) => b @ [a]) [] (Byte.stringToBytes inAddrStr)
            val toSend = (Word8VectorSlice.full o Word8Vector.fromList) (arr1 @ arr2 @ portArr)
          in
            (sendVec (sock, toSend); ())
          end
      fun connectAndPiping sock addr port =
        let val sockfd = INetSock.TCP.socket ()
            val aSock = Asio.Socket.socket (sockfd, lp)
            val _ = case NetHostDB.getByName addr of
                      NONE => raise Fail "Unable to resolve"
                    | SOME en => (
                        print ("Connecting to " ^ ((NetHostDB.toString o NetHostDB.addr) en) ^ "\n");
                        Asio.Socket.connect (
                        aSock, INetSock.toAddr (
                            (NetHostDB.addr en), port
                        )
                      ))
            val _ = sendServerRep sock sockfd
            fun fromClient () =
              let val vec = recvSomeVec (sock, 65536)
                  (* val _ = print ("received from clinet " ^ (Int.toString (Word8VectorSlice.length vec)) ^ "\n") *)
              in vec
              end

            fun fromRemote () = Asio.Socket.recvSomeVec (aSock, 65536)
            fun toClient vec =
              let val n = sendVec (sock, vec)
                  (* val _ = print ("sent to clinet " ^ (Int.toString n) ^ "\n") *)
              in n
              end

            fun toRemote vec = Asio.Socket.sendVec (aSock, vec)
            val _ = Thread.spawn (fn () =>
              doPiping fromClient toRemote
              handle SocketClosed => (Asio.Socket.close sock; Asio.Socket.close aSock))
        in doPiping fromRemote toClient
            handle SocketClosed => (Asio.Socket.close sock; Asio.Socket.close aSock)
        end

      fun socks5Thread sock =
        let val (ver, nmethods, methods) = recvClientHello sock
            val _ = print ("Socks Version " ^ (Word8.toString ver) ^ "\n" )
            val _ = sendServerHello sock (0w5, 0w0)
            val (cmd, addr, port) = recvClientReq sock
            val _ = print ((CmdTypeToString cmd) ^ " " ^ addr ^ " : " ^ (Int.toString port) ^ "\n" )
            fun handleReq cmd addr port =
              case cmd of
                CONNECT => connectAndPiping sock addr port
              | _       => print "Unsupported command\n"
        in
          (handleReq cmd addr port)
        end
        handle _ => Asio.Socket.close sock
    end
    val _ = socks5Thread sock
    val _ = Evp.Aes.free dctx
    val _ = Evp.Aes.free ectx
  in
    ()
  end
  (* fun connectionThread sock =
    let fun work () =
          let val vec = recvVec (sock, slice)
              val _ = print ("Received " ^ (Int.toString len) ^ " bytes from client\n")
              val toSend = Word8VectorSlice.slice (vec, 0, SOME len)
              val _ = sendVec (sock, toSend)
              val _ = print ("Sent " ^ (Int.toString len) ^ " bytes to cleint\n")
          in if len <> 0 then work ()
             else ()
          end
    in (work (); MLton.GC.collect ())
    end *)

fun acceptThread () =
  let val (newSock, addr) = Asio.Socket.accept listenSocket
      val (inAddr, port) = INetSock.fromAddr addr
      val _ = print ("New Connection " ^ (NetHostDB.toString inAddr) ^
                     " : " ^ (Int.toString port) ^ " " ^ "\n")
      fun spawnHandler false = Thread.spawn (fn () => (ServerThread o Asio.Socket.socket)
                                      (newSock, lp))
        | spawnHandler true  = Thread.spawn (fn () => (tunnelThread o Asio.Socket.socket)
                                        (newSock, lp))
      val _ = spawnHandler tunnelMode
  in acceptThread () end

val _ = Thread.spawn acceptThread
val _ = Asio.EventLoop.run lp
