(*val _ =
   case Posix.Process.fork () of
      NONE =>
         let
            val _ = Posix.Process.sleep (Time.fromSeconds 1)
            val (socket, _) = Socket.accept socket
            val _ = print (read socket)
            val _ = print (case readNB socket of
                              NONE => "NONE\n"
                            | SOME s => s)
            val _ = write (socket, "goodbye, world\n");
            val _ = Socket.close socket
         in
            ()
         end
    | SOME pid =>
         let
            val socket' = INetSock.TCP.socket ()
            val _ = Socket.connect (socket', addr)
            val _ = write (socket', "hello, world\n")
            val _ = print (read socket')
            val _ = Socket.close socket'
            val (pid', status)  = Posix.Process.wait ()
         in
            if pid = pid' andalso status = Posix.Process.W_EXITED
               then ()
            else print "child failed\n"
         end *)

datatype ('af,'sock_type) session = Session of {
    socket: ('af, 'sock_type) Socket.sock,
    addr: string,
    port: int,
    count: int ref,
    pollFd: OS.IO.poll_desc ref,
    readHandler : ('af, 'sock_type) session -> bool,
    writeHandler : ('af, 'sock_type) session -> bool,
    exceptHandler : ('af, 'sock_type) session -> bool
}

exception Failure;
fun getSessionKey (Session ss) : OS.IO.iodesc
    = Socket.ioDesc (#socket ss)

signature SESSION_TABLE =
    sig
        type ('af, 'sock_type) t
        type key = OS.IO.iodesc
        type ('af,'sock_type) value = ('af,'sock_type) session
        val new: unit -> ('af,'sock_type) t
        val peek: ('af,'sock_type) t * key -> ('af,'sock_type) value option
        val remove: ('af,'sock_type) t * key -> unit
        (* val removeAll: t -> unit *)
        val size: ('af,'sock_type) t -> int
        val toList: ('af,'sock_type) t -> (key * ('af,'sock_type) value) list
        val insert: ('af,'sock_type) t * key * ('af,'sock_type) value ->
            ('af,'sock_type) value
    end


fun read socket : string =
   Byte.unpackStringVec (Word8VectorSlice.full (Socket.recvVec (socket, 100)))

fun readNB socket : string * bool =
    let
        val arraySlice = Word8ArraySlice.full (Word8Array.array (1, Word8.fromInt 0))
        fun loop socket = case Socket.recvArrNB (socket, arraySlice) of
                            SOME size => if size > 0 then
                                            let val str = (Byte.unpackString o
                                                    Word8ArraySlice.subslice) (arraySlice, 0, SOME size)
                                                val (left, error) = loop socket
                                            in
                                                (str ^ left, error)
                                            end
                                        else ("", true)
                          | NONE => ("", true)
    in
        loop socket
    end


fun write (socket, s: string): unit =
   (Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes s))
    ; ())

fun handleClient sock addr =
    let
        val (inAddr, port) = INetSock.fromAddr addr
        fun rhandle (Session ss) =
            let
                val _ = print ("read handler\n")
                val _ = print ("port " ^ (Int.toString (#port ss)) ^ "\n")
                val sock = #socket ss
                val (str, error) = readNB sock
                val _ = print str
                val count = ref (#count ss)
                val _ = !count := !(!count) + (String.size str)
                val _ = print ("received " ^ (Int.toString (!(!count))) ^ "\n")
            in
                not error
            end
        fun whandle (Session ss) =
            let
                val _ = print ("write handler\n")
                val sock = #socket ss
                val _ = write (sock, "Hello World\n")
            in
                true
            end
        fun makeFD sock = (OS.IO.pollIn o OS.IO.pollPri o
            (fn ioDesc => case OS.IO.pollDesc ioDesc of
                            SOME fd => fd
                          | NONE => raise Failure) o Socket.ioDesc) sock
    in
        {
            socket = sock,
            addr = NetHostDB.toString inAddr,
            port = port,
            count = ref 0,
            pollFd = ref (makeFD sock),
            readHandler = rhandle,
            writeHandler = whandle,
            exceptHandler = fn _ => false
        }
    end

fun mapPartial pred l =
   rev (foldl (fn (x, l) => (case pred x of
                              NONE => l
                            | SOME y => y :: l))
              [] l)

structure SessionTable :> SESSION_TABLE =
    struct
        type key = OS.IO.iodesc
        type ('af,'sock_type) value = ('af,'sock_type) session
        type ('af, 'sock_type) t = (key, ('af, 'sock_type) session) HashTable.t

        fun new () = HashTable.new {
            equals = fn (x, y) => OS.IO.compare (x, y) = EQUAL,
            hash = fn ioDesc => OS.IO.hash ioDesc
        }

        fun peek (table, key) = HashTable.peek (table, key)
        fun remove (table, key) = HashTable.remove (table, key)
        fun insert (table, key, ss) = HashTable.insertIfNew (table, key,
            fn _ => ss, fn _ => ())
        fun size (table) = HashTable.size (table)
        fun toList (table) = (HashTable.toList (table))
    end


fun main () : unit =
    let
        val sockfd = INetSock.TCP.socket ()
        val _ = Socket.bind (sockfd, INetSock.any 1400)
        val _ = Socket.listen (sockfd, 5)

        fun doAccept table =
            let
                fun insertNewSession (table, ss) =
                        SessionTable.insert(table, getSessionKey ss, ss)

                val _ = case Socket.acceptNB sockfd of
                          NONE => ()
                        | SOME (newSock, addr) =>
                            let
                                val (inAddr, port) = INetSock.fromAddr addr
                                val _ = print ("New Connection " ^ (NetHostDB.toString inAddr) ^
                                               " : " ^ (Int.toString port) ^ " " ^ "\n")
                                val newSession = handleClient newSock addr
                                val _ = insertNewSession (table, Session newSession)
                            in
                                ()
                            end
            in
                ()
            end

        val pollFd = case OS.IO.pollDesc (Socket.ioDesc sockfd) of
                        SOME fd => OS.IO.pollIn fd
                     |  _       => raise Failure

        val sessionTable = SessionTable.new ()

        fun acceptForever table =
            let
                val _ = print ("Start polling\n")
                val pollDescList = pollFd :: (map (fn (_, Session ss) => !(#pollFd ss))
                                   (SessionTable.toList sessionTable))
                val _ = print ("Desc list " ^ Int.toString (length pollDescList) ^ "\n")
                val pollInfoList = OS.IO.poll (pollDescList, NONE)
                val _ = print ("Poll info " ^ Int.toString (length pollInfoList) ^ "\n")

                fun handler poll_info =
                    let
                        val ioDesc = (OS.IO.pollToIODesc o OS.IO.infoToPollDesc) poll_info
                        val isIn = OS.IO.isIn poll_info
                        val isOut = OS.IO.isOut poll_info
                        val isPri = OS.IO.isPri poll_info
                        val anyEvents = isIn orelse isOut orelse isPri
                        fun pollInfoToIODesc info = OS.IO.pollToIODesc (OS.IO.infoToPollDesc info)
                    in
                        if not anyEvents then (NONE, true)
                        else case SessionTable.peek (table, ioDesc) of
                               NONE => if OS.IO.compare (OS.IO.pollToIODesc pollFd,
                                                         pollInfoToIODesc poll_info) = EQUAL
                                       then (doAccept table; (NONE, true))
                                       else (NONE, true)
                             | SOME (Session ss) =>
                                case Socket.Ctl.getERROR (#socket ss) of
                                  true => (SOME (Session ss), false)
                                | false => let  val poll_desc = OS.IO.infoToPollDesc poll_info
                                                val e1 = if isIn
                                                         then (#readHandler ss) (Session ss)
                                                         else true
                                                val e2 = if isOut
                                                         then (#writeHandler ss) (Session ss)
                                                         else true
                                                val e3 = if isPri
                                                         then (#exceptHandler ss) (Session ss)
                                                         else true
                                                in (SOME (Session ss),
                                                    e1 andalso e2 andalso e3)
                                            end
                    end


                val _ = map (fn info => let
                                            val (session, success) = handler info
                                        in
                                            case success of
                                              true  => ()
                                            | false => (case session of
                                                         SOME (Session ss) =>
                                                         (SessionTable.remove (table, getSessionKey (Session ss)); ())
                                                       | NONE => ())
                                        end) pollInfoList
                val _ = print ("one round\n")
            in
                acceptForever table
            end
    in
        acceptForever sessionTable
    end

val _ = main ()
