import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = Socket -> SockAddr -> String -> IO ()

server :: String              -- ^ Port number or name; 514 is default
       -> HandlerFunc         -- ^ Function to handle incoming messages
       -> IO ()
server port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bind sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc sock addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler sock addr msg = do
    let output = "From " ++ show addr ++ ": " ++ msg
    putStrLn output
    sendTo sock output addr
    return ()

main :: IO ()
main = server "6666" plainHandler
