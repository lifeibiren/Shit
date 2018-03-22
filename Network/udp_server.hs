import Data.Bits
import Network.Socket
import Network.BSD
--import Data.List
import Data.Map.Strict
import Data.Time.Clock

type HandlerFunc = Socket -> SockAddr -> String -> UserMap -> IO ()
type User = SockAddr
type UserMap = Map User UTCTime

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
       procMessages sock empty
    where procMessages sock userMap =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 now <- getCurrentTime
                 let filterFunc  = removeDead now 60 -- 60s time out
                     newUserMap  = insert addr now userMap
                     filteredMap = foldrWithKey filterFunc empty newUserMap
                 -- Handle it
                 handlerfunc sock addr msg filteredMap
                 -- And process more messages
                 procMessages sock filteredMap

-- Strip timed out clients
removeDead :: UTCTime -> NominalDiffTime -> SockAddr -> UTCTime -> UserMap -> UserMap
removeDead now timeout key val result
    | past > timeout = result 
    | otherwise      = insert key val result 
    where past = diffUTCTime now val

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler sock addr msg userMap = do
    let output = "From " ++ show addr ++ ": " ++ msg
    putStrLn output
    
    let ret = foldrWithKey concatMsg "" userMap 
    sent <- sendTo sock ret addr
    putStrLn ret

    where concatMsg key _ result = show key ++ "\n" ++ result

main :: IO ()
main = server "6666" plainHandler
