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
                                        else ("", false) (* EOF *)
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
        fun makeFD sock (r,w,p) =
            let
                fun makeIn x = if r then OS.IO.pollIn x else x
                fun makeOut x = if w then OS.IO.pollOut x else x
                fun makePri x = if p then OS.IO.pollPri x else x
                fun ioDesc sock = case OS.IO.pollDesc (Socket.ioDesc sock) of
                                 SOME fd => fd
                               | NONE => raise Failure
            in
                (makeIn o makeOut o makePri o ioDesc) sock
            end
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
                val _ = (#pollFd ss) := makeFD (#socket ss) (true, true, false)
            in
                true
            end
        fun whandle (Session ss) =
            let
                val _ = print ("write handler\n")
                val sock = #socket ss
                val _ = write (sock, "Hello World\n")
                val _ = (#pollFd ss) := makeFD (#socket ss) (true, false, false)
            in
                true
            end
    in
        {
            socket = sock,
            addr = NetHostDB.toString inAddr,
            port = port,
            count = ref 0,
            pollFd = ref (makeFD sock (true, false, false)),
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

                fun handler pollInfo =
                    let
                        val ioDesc = (OS.IO.pollToIODesc o OS.IO.infoToPollDesc) pollInfo
                        val isIn = OS.IO.isIn pollInfo
                        val isOut = OS.IO.isOut pollInfo
                        val isPri = OS.IO.isPri pollInfo
                        val anyEvents = isIn orelse isOut orelse isPri
                        fun pollInfoToIODesc info = OS.IO.pollToIODesc (OS.IO.infoToPollDesc info)

                        fun callHandler pollInfo ss =
                            let
                                val poll_desc = OS.IO.infoToPollDesc pollInfo
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
                    in
                        if not anyEvents then (NONE, true)
                        else case SessionTable.peek (table, ioDesc) of
                               NONE => if OS.IO.compare (OS.IO.pollToIODesc pollFd,
                                                         pollInfoToIODesc pollInfo) = EQUAL
                                       then (doAccept table; (NONE, true))
                                       else (NONE, true)
                             | SOME (Session ss) =>
                                case Socket.Ctl.getERROR (#socket ss) of
                                  true => (SOME (Session ss), false)
                                | false => callHandler pollInfo ss
                    end

                val _ = map (fn info =>
                    let
                        val (session, success) = handler info
                    in
                        case success of
                          true  => ()
                        | false => (case session of
                                     SOME (Session ss) =>
                                     (SessionTable.remove (table,
                                         getSessionKey (Session ss)); ())
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
