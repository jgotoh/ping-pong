module Main where

import qualified Control.Distributed.Process.Internal.Messaging as M
import qualified Control.Distributed.Process.Extras.SystemLog as Log
import Control.Distributed.Process.Internal.Types
import qualified Control.Distributed.Process   as P
import qualified Network.Transport             as T
import qualified Control.Distributed.Process.Node
                                               as Node
import           Data.ByteString.Char8
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Network.Transport.TCP         as NT
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

processName = "testProcess"

main :: IO ()
main = do
  cfg <- parseConfig
  print cfg
  case cfg of
    StartConfig ip p      -> startProcess ip p
    JoinConfig ip p j raw -> joinProcess ip p j raw

-- start process, register, wait for message, quit
startProcess :: String -> Int -> IO ()
startProcess ip port = do
  (Right trans) <- NT.createTransport (NT.defaultTCPAddr ip (show port))
                                      NT.defaultTCPParameters
  node <- Node.newLocalNode trans Node.initRemoteTable
  Node.runProcess node $ do
    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return
    pid <- P.getSelfPid
    P.registerRemoteAsync (Node.localNodeId node) processName pid
    let serverAddress = P.nodeAddress $ Node.localNodeId node
    P.liftIO $ print $ "waiting for message at: " ++ show serverAddress
    msg <- P.expect :: P.Process String
    P.liftIO $ print $ "received message: " ++ msg

-- search for started process, send message, quit
joinProcess :: String -> Int -> String -> Bool -> IO ()
joinProcess ip port join sendRaw = do
  (Right trans) <- NT.createTransport (NT.defaultTCPAddr ip (show port))
                                      NT.defaultTCPParameters
  node <- Node.newLocalNode trans Node.initRemoteTable
  Node.runProcess node $ do

    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

    -- whereisRemoteAsync uses sendCtrlMsg, try whether other CtrlMsgs are sendable/ received
    -- M.sendCtrlMsg (pure joinNode) (GetNodeStats joinNode)
    -- M.sendCtrlMsg (pure joinNode) (SigShutdown)

    -- try to connect directly via socket: works.
    if sendRaw
      then do
        Right localEndpoint <- P.liftIO $ T.newEndPoint trans
        Right conn          <- P.liftIO $ T.connect localEndpoint
                                                    joinEndpointAddr
                                                    T.ReliableOrdered
                                                    T.defaultConnectHints
        Right () <- P.liftIO $ T.send conn [pack "raw hello"]
        P.liftIO $ print "successfully sent raw hello"
      else do
    -- try to find registered process: does not work
        P.liftIO $ print "searching process"
        (Just pid) <- searchProcessTimeout processName joinNode 5000
        P.liftIO $ print "found process, sending message"
        P.send pid "hello"

 where
  joinEndpointAddr = T.EndPointAddress $ pack join
  joinNode         = P.NodeId joinEndpointAddr

searchProcessTimeout
  :: String -> P.NodeId -> Int -> P.Process (Maybe P.ProcessId)
searchProcessTimeout name addr timeLeft
  | timeLeft <= 0 = do
    P.liftIO
      $  print
      $  "searchProcess ended due to timeout. No process "
      ++ name
      ++ " at address "
      ++ show addr
      ++ " could be found"
    return Nothing
  | otherwise = do
    P.whereisRemoteAsync addr name
    reply <- P.expectTimeout timeout
    case reply of
      Just (P.WhereIsReply name' maybeID) -> case maybeID of
        Just pid -> do
          P.liftIO $ print $ "WhereIsReply " ++ name' ++ " pid: " ++ show pid
          return $ Just pid
        Nothing -> do
          P.liftIO $ print "got a reply, but no existing PID"
          searchProcessTimeout name addr (timeLeft - timeoutMS)
      Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
 where
  timeoutMS = 1000
  timeout   = Time.asTimeout $ Time.milliSeconds timeoutMS

data Config = JoinConfig {ip :: String, port :: Int, join :: String, raw :: Bool}
            | StartConfig {ip :: String, port :: Int}
  deriving (Show)

parseConfig :: IO Config
parseConfig = execParser $ info (configParser <**> helper) fullDesc

joinConfigParser :: Parser Config
joinConfigParser =
  JoinConfig
    <$> strOption (long "ip" <> help "IP of joining process" <> metavar "IP")
    <*> option auto (long "p" <> help "Port of joining" <> metavar "PORT")
    <*> strOption (long "j" <> help "EndpointAddress to join" <> metavar "JOIN")
    <*> switch
          (long "raw" <> help
            "Set flag to connect via TCP instead of searching for a process"
          )

startConfigParser :: Parser Config
startConfigParser =
  StartConfig
    <$> strOption
          (long "startip" <> help "IP of hosting process" <> metavar "IP")
    <*> option
          auto
          (long "startp" <> help "Port of hosting process" <> metavar "PORT")

configParser :: Parser Config
configParser = startConfigParser <|> joinConfigParser

