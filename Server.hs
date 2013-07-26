module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad

import Data.List (delete)
import Data.Maybe
import Data.Word

import Network

import System.IO
import System.Environment

import Text.Printf


defaultPort = 4711 :: Word16

data Client = Client {
    clientHandle :: Handle
  , clientHost :: HostName
  , clientPort :: PortNumber
  , userName :: String
  , tries :: Int
  , state :: CState
 }

data Object = Object {
    objectColor :: String
  , sposX :: String
  , sposY :: String
  , eposX :: String
  , eposY :: String
  , shape :: String
}

instance Eq Client where
  a == b = clientHandle a == clientHandle b

instance Show Client where
  show client = printf "%s:%s" (clientHost client) (show $ clientPort client)

data CState = Draw | Guess | Blocked | Uninitialized

data Action = Send Client String | Register Client | Unregister Client

mkClient :: (Handle, HostName, PortNumber) -> Client
mkClient (conn, host, port) = Client conn host port "Default" 5 Guess

main = withSocketsDo $ do
    port    <- maybe defaultPort read <$> listToMaybe <$> getArgs
    socket  <- listenOn (PortNumber (fromIntegral port))

    control <- newChan
    
    forkIO $ dispatcher control [] []

    forever $ do
        client <- mkClient <$> accept socket
        writeChan control (Register client)
        forkIO $ serve control client

serve ctrl client = do
    let conn = clientHandle client
    isEOF <- hIsEOF conn
    if isEOF
        then writeChan ctrl (Unregister client) >> hClose conn
        else hGetLine conn >>= writeChan ctrl . Send client >> serve ctrl client
    
dispatcher ctrl clients objects = do
    action   <- readChan ctrl
    clients' <- case action of

        Send from str -> do
            printf "%s sends: %s\n" (show from) (show str)
            case str of
              "[User-Name]:" -> case (state from) of
                Uninitialized -> return $ delete from clients : from' 
                  where from' = if clients == [] then from {state = Draw, userName = str} else from {state = Guess, userName = str} -- Hier muss man den UserName natürlich vorher auspacken!
                _ -> return $ delete from clients : from'
                  where from' = from {userName = str} -- Hier muss der UserName natürlich ausgepackt werden
              "[Guess]" -> return clients
              "[Chat]" -> return clients
              "[Draw]" -> return clients
              _ -> return clients
            {- mapM (send str) (delete from clients) -}

        Register client -> do
            printf "%s entered the game\n" (show client)
            return $ client' : clients
              where client' = client { state = Uninitialized } 

        Unregister client -> do
            printf "%s left the game\n" (show client)
            return $ delete client clients

    dispatcher ctrl clients' objects

send message client = do
    let conn = clientHandle client
    hPutStrLn conn message
    hFlush conn
