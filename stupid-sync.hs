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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.Word
import Data.Char
import Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Objects to be synced are identified by the file path
-- relative to the storage directory.
-- (The path shall undergo security checks before being
-- wrapped in SyncObject.)
newtype SyncObject = SyncObject FilePath
  deriving (Show, Eq, Ord)

validateSyncObjectPath :: BS.ByteString -> Maybe SyncObject
validateSyncObjectPath path = do
  parts <- BS.split (word8 '/')  <$> BS.stripPrefix "/" path
  guard $ all ($ parts) objectPathChecks
  return . SyncObject . joinPath $
    map (fromString . BS.Char8.unpack) parts

word8 :: Char -> Word8
word8 = fromIntegral . ord

chr' :: Word8 -> Char
chr' = chr . fromIntegral

objectPathChecks :: [[BS.ByteString] -> Bool]
objectPathChecks =
  [ not . null
  , (<=20) . length
  , all (not . BS.null)
  , all ((<=100) . BS.length)
  , all (all allowedChar . BS.Char8.unpack)
  , all (not . BS.isPrefixOf ".")
  ]

allowedChar :: Char -> Bool
allowedChar = flip Set.member . Set.fromList $
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '-', '_']

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
  let s = ServerState Map.empty 0 dir
  state <- newMVar s
  runServer "127.0.0.1" 39141 (app state)

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
sendInitialSyncData state (SyncObject soPath) conn = do
  s <- readMVar state
  syncData <- BS.readFile (dataDir s </> soPath)
    `catch` (\e -> if isDoesNotExistError e then return BS.empty
                   else throwIO e)
  sendBinaryData conn syncData

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
saveData s (SyncObject path) syncData = do
  let fullPath = dataDir s </> path
  createDirectoryIfMissing True (takeDirectory fullPath)
  BS.writeFile fullPath syncData
