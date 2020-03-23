{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.WebSockets as WS
import System.IO.Error
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Functor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Objects to be synced are identified by the file name.
-- (The name must undergo security checks before being
-- wrapped in SyncObject.)
newtype SyncObject = SyncObject FilePath
  deriving (Show, Eq, Ord)

validateSyncObjectPath :: BS.ByteString -> Maybe SyncObject
validateSyncObjectPath path = do
  name <- BS.Char8.unpack <$> stripPrefix "/" path
  guard $ all ($ name) objectNameChecks
  return (SyncObject name)

-- drop-in for BS.stripPrefix in bytestring >= 0.10.8
stripPrefix :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
stripPrefix a b =
  if a `BS.isPrefixOf` b then Just (BS.drop (BS.length a) b)
  else Nothing

objectNameChecks :: [String -> Bool]
objectNameChecks =
  [ not . null
  , (<=100) . length
  , all isAllowedChar
  ]

isAllowedChar :: Char -> Bool
isAllowedChar = (`Set.member` setOfAllowedChars)

setOfAllowedChars :: Set.Set Char
setOfAllowedChars = Set.fromList $
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']

type ClientID = Int

data ServerState = ServerState
  { clients :: Map.Map SyncObject (Map.Map ClientID Connection)
  , nextClientID :: ClientID
  , dataDir :: FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  when (null args) exitWrongArguments
  let dir = head args
  validDir <- doesDirectoryExist dir
  when (not validDir) exitWrongArguments
  putStrLn ("using data directory " ++ dir)
  let s = ServerState Map.empty 0 dir
  state <- newMVar s
  runServer addr port (app state)
  where
    addr = "127.0.0.1"
    port = 39141

exitWrongArguments :: IO ()
exitWrongArguments = do
  putStrLn "give data directory as first argument"
  exitFailure

app :: MVar ServerState -> ServerApp
app state pendingConn = do
  let path = requestPath (pendingRequest pendingConn)
  case validateSyncObjectPath path of
    Nothing -> do
      putStrLn $ "Rejecting request for " ++ BS.Char8.unpack path
      rejectRequest pendingConn (BS.Char8.pack "Path not accepted.")
    Just syncObject -> do
      putStrLn $ "Accepting request for " ++ BS.Char8.unpack path
      conn <- acceptRequest pendingConn
      clientID <- addClient state syncObject conn
      putStrLn $ "(clientID " ++ show clientID ++ ")"
      ( do
        sendInitialSyncData state syncObject conn
        listenTo state syncObject conn )
        `finally` removeClient state syncObject clientID

-- Send sync data from file if existent.
sendInitialSyncData ::
  MVar ServerState -> SyncObject -> Connection -> IO ()
sendInitialSyncData state so conn = do
  s <- readMVar state
  syncData <- BS.readFile (syncObjectFilePath s so)
    `catch` (\e -> if isDoesNotExistError e then return BS.empty
                   else throwIO e)
  sendBinaryData conn syncData

syncObjectFilePath :: ServerState -> SyncObject -> FilePath
syncObjectFilePath s (SyncObject soName) = dataDir s </> soName

-- Add new client to the list and return the new id.
addClient :: MVar ServerState -> SyncObject -> Connection -> IO ClientID
addClient state so conn = modifyMVar state $ \s -> return $
  let id = nextClientID s
      inner = Map.findWithDefault Map.empty so (clients s)
      clients' = Map.insert so (Map.insert id conn inner) (clients s)
  in (s { clients = clients', nextClientID = id+1 }, id)

-- Remove client from list.
removeClient :: MVar ServerState -> SyncObject -> ClientID -> IO ()
removeClient state so id = do
  putStrLn ("Removing client " ++ show id)
  modifyMVar_ state $ \s -> return $
    let inner = Map.findWithDefault Map.empty so (clients s)
        clients' = Map.insert so (Map.delete id inner) (clients s)
    in s { clients = clients' }

-- Update and relay sync data from this client.
listenTo :: MVar ServerState -> SyncObject -> Connection -> IO ()
listenTo state so conn = forever $ do
  syncData <- receiveData conn
  s <- readMVar state
  broadcastData s so syncData
  saveData s so syncData

broadcastData :: ServerState -> SyncObject -> BS.ByteString -> IO ()
broadcastData s so syncData =
  let clientsMap = Map.findWithDefault Map.empty so (clients s)
  in forM_ (Map.elems clientsMap) $ \conn ->
    sendBinaryData conn syncData

saveData :: ServerState -> SyncObject -> BS.ByteString -> IO ()
saveData s so syncData =
  BS.writeFile (syncObjectFilePath s so) syncData
