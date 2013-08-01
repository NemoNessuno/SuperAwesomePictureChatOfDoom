module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad

import Data.List (delete)
import Data.Maybe
import Data.Word
import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Conc.Sync

import Network

import System.IO
import System.Environment

import Text.Printf


defaultPort = 4711 :: Word16

data Client = Client {
    clientHandle :: Handle
  , clientHost :: HostName
  , clientPort :: PortNumber
 }

instance Eq Client where
  a == b = clientHandle a == clientHandle b

instance Show Client where
  show client = printf "%s:%s" (clientHost client) (show $ clientPort client)

data Action = Add Client Text Text | LookUp Client Text | Register Client | Unregister Client | NOP

mkClient :: (Handle, HostName, PortNumber) -> Client
mkClient (conn, host, port) = Client conn host port

main = withSocketsDo $ do
    port     <- maybe defaultPort read <$> listToMaybe <$> getArgs
    socket   <- listenOn (PortNumber (fromIntegral port))
    
    storeVar <- atomically $ newTVar (Map.empty :: Map Text Text)
    control  <- newChan
    
    forkIO $ dispatcher control [] storeVar

    forever $ do
        client <- mkClient <$> accept socket
        writeChan control (Register client)
        forkIO $ serve control client

serve ctrl client = do
    let conn = clientHandle client
    isEOF <- hIsEOF conn
    if isEOF
        then writeChan ctrl (Unregister client) >> hClose conn
        else hGetLine conn >>= writeChan ctrl . getAction client >> serve ctrl client

dispatcher :: Chan Action -> [Client] -> TVar (Map Text Text) -> IO ()
dispatcher ctrl clients storeVar = do
    action   <- readChan ctrl
    clients' <- case action of

        LookUp from key -> do
            printf "%s looks up: %s\n" (show from) (show key)
            store <- readTVarIO storeVar
            let answer = case Map.lookup key store of
                          Just a -> (unpack key) ++ " is " ++ (unpack a)
                          _      -> "Key was not found!"
            send answer from
            return clients

        Add from key value -> do
            printf "%s adds key :%s with value: %s\n" (show from) (show key) (show value)
            atomically $ readTVar storeVar >>= return . Map.insert key value >>= writeTVar storeVar
            send ("added key: " ++ (unpack key) ++ " with value: " ++ (unpack value)) from
            return clients

        Register client -> do
            printf "%s entered\n" (show client)
            return $ client : clients

        Unregister client -> do
            printf "%s left\n" (show client)
            return $ delete client clients

        NOP -> do
            return clients

    dispatcher ctrl clients' storeVar

send message client = do
    let conn = clientHandle client
    hPutStrLn conn message
    hFlush conn

getAction :: Client -> String -> Action
getAction client string = let splitText = split (== '=') (pack string) in
                          case splitText of 
                            [a]   -> LookUp client a
                            [a,b] -> Add client a b
                            _     -> NOP
