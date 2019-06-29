{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Socket
  ( runSocketServer
  )
where

import           Control.Concurrent hiding (yield)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS
import           Prelude
import           Web.JWT                        ( Secret )

import           Socket.Clients
import           Socket.Concurrency
import           Socket.Lobby
import           Socket.Msg
import           Socket.Setup
import           Socket.Subscriptions
import           Socket.Types
import           Socket.Workers
import           Types

import           Control.Lens            hiding ( Fold )
import           Database
import           Poker.Types             hiding ( LeaveSeat )
--import           Data.Traversable


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as X
import qualified Data.Text.Lazy.Encoding       as D
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
--import           Control.Monad.State.Lazy hiding (evalStateT)
import qualified Data.ByteString.Lazy          as BL

import           Socket.Types
import qualified Data.List                     as L
import qualified Data.Text.Lazy                as X

import qualified Data.Text.Lazy.Encoding       as D
import           System.Random

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket.Types
import           Socket.Msg
import           Socket.Utils
import Data.ByteString.UTF8 (fromString)

import           Poker.Poker
import           Crypto.JWT
import qualified Data.Aeson as A

import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
--import           Data.Conduit                   ( Conduit
--                                                , runConduitRes
--                                                , yieldM
--                                                , (.|)
--                                                )
--
--import qualified Data.Conduit.List             as CL
-- import           Conduit
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Poker.Types

import Control.Monad 
import Control.Exception
import qualified GHC.IO.Exception as G

import qualified Network.WebSockets            as WS

import Pipes.Aeson
import           Pipes                         
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import Pipes.Parse hiding (decode, encode)
import qualified Pipes.Prelude                 as P

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState { clients = M.empty, lobby = lobby }

-- Create the initial lobby holding all game state and then fork a new thread for each table in the lobby
-- to write new game states to the DB
runSocketServer
  :: BS.ByteString -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port connString redisConfig = do
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ TakeSeat "black" 2222)
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ GameMove "black" $ PlayerAction {name = "player1", action = Raise 400})
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ GameMove "black" $ PlayerAction {name = "player1", action = Fold})


  lobby           <- initialLobby
  serverStateTVar <- atomically $ newTVar $ initialServerState lobby
  forkBackgroundJobs connString serverStateTVar lobby
  print $ "Socket server listening on " ++ (show port :: String)
  _ <- forkIO $ WS.runServer "0.0.0.0" port $ application secretKey
                                                          connString
                                                          redisConfig
                                                          serverStateTVar
--  _ <- forkIO $ delayThenSeatPlayer connString 1000000 serverStateTVar bot1
--  _ <- forkIO $ delayThenSeatPlayer connString 2000000 serverStateTVar bot2
  --_ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot3
 -- _ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot4
 -- _ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot5
 -- threadDelay 100000 --delay so bots dont start game until all of them sat down
  --_ <- forkIO $ startBotActionLoops connString serverStateTVar playersToWaitFor botNames
  return ()
 where
  botNames         = (^. playerName) <$> [bot1, bot2]
  playersToWaitFor = length $ botNames

-- subscriptions are handled by combining each subscribers mailbox into one large mailbox
-- where mew MsgOuts with new game states are posted
--
-- The new game state msgs will then propogate to to the subscribers mailbox and 
-- sent via their websocket connection automatically
subscribeToTable :: Output MsgOut -> Output MsgOut -> Output MsgOut
subscribeToTable tableOutput playerOutput = tableOutput <> playerOutput


-- Uses incoming msg mailbox as a producer of actions which are run against 
-- the game state and then fed into the outgoing msg mailbox to be propagated to clients
--gameStream :: Input GameMsgIn -> Output GameMsgOut -> Output GameMsgOut
--gameStream msgInput msgOutput = producer >-> gameMsgInHandler' >-> consumer 
--  where 
--    producer = fromInput msgInput
--    consumer = toOutput msgOutput



-- (~>) :: Functor m => (a -> Producer b m r) -> (b -> Producer   c m ()) -> (a -> Producer   c m r)


--s :: Producer GameMsgOut IO () -> Producer MsgOut IO ()
--s returnToSender propagateValidAction = 
--  either  (toOutput senderMsgMailbox) updateGame

-- Note this doesn't propagate new game state to clients just updates the game in the lobby
updateGame :: TVar ServerState -> TableName -> Game -> STM ()
updateGame s tableName g = do 
  ServerState{..} <- readTVar s
  let newLobby = updateTableGame tableName g lobby
  writeTVar s ServerState { lobby = newLobby , .. }

-- and writes GameOutMsg to outgoing mailbox in table so it is propagated to clients
--broadcast'
--broadcast' 
--



evalPlayerAction :: Game -> Pipe PlayerAction (Either GameErr Game) IO (Either GameErr Game)
evalPlayerAction g = forever $ do 
    playerAction <- await
    res <- lift $ runPlayerAction g playerAction
    yield res


-- An illegal player moves result in the err being sent back to the client who made the move
-- whereas if the player move is legal we get a new game state (Right) which is written to  
-- the table mailbox for propagation to clients who are observing the game
-- (~>) composes our consumers which handle the success and failure cases of 
-- player action evaluation. This composition returns a new consumer which abstracts
-- away the machinery of handling failure for illegal player actions.
playerActionHandler' ::
     (GameErr -> Consumer PlayerAction IO ())
  -> (Game -> Consumer PlayerAction IO ())
  -> Game
  -> Consumer PlayerAction IO (Either GameErr Game)
playerActionHandler' failureConsumer successConsumer =
    evalPlayerAction ~> divertResult
    where
      divertResult = 
        \case 
          Left e  -> failureConsumer e
          Right g -> successConsumer g


-- When a valid player action is received the resulting MsgOut after evaluation against the game
-- is diverted to the game msg mailbox so new gamestate so it can be propagated
-- to all observers.
--
-- Otherwise if the evaluation of the player action failed i.e invalid move then 
-- the error msg is diverted back to the player's socket connection mailbox 
--gg ::
--  Output GameMsgOut ->
--  Output GameMsgOut -> Producer PlayerAction IO () -> Effect IO ()
--gg pOut gOut producer = producer >-> actionHandler playerAction


--runEffect $ actionProducer >->

-- errConsumer :: Consumer MsgOut IO ()
-- errConsumer = toOutput playerMsgOutSink
-- gameUpdateConsumer :: Consumer GameMsgOut IO ()
-- gameUpdateConsumer = toOutput gameMsgOutSink
  
--playerActionHandler errConsumer gameUpdateConsumer 
--(~>) :: (a -> Pipe   x b m r) -> (b -> Consumer x   m ()) -> (a -> Consumer x   m r)

--for' ::
--  Pipe PlayerAction (Either GameErr Game) m r -> 
--  (Either GameErr Game -> Consumer PlayerAction m ()) -> 
--  Consumer PlayerAction m r
--for' p h = for p h 

actionHandler :: Game -> Pipe PlayerAction (Either GameErr Game) IO ()
actionHandler g = forever $ do
  a <- await
  res <- lift $ runPlayerAction g a
  yield res


-- playerActionHandler :: Output GameMsgOut -> Output MsgOut -> 
  -- playerActionHandler gameMailbox playerMailbox =
--    ::IO (Either GameErr Game)
--  either each
---- each :: (Monad m, Foldable f) => f a -> Producer a m ()


--gameMsgOutHandler' :: Pipe (Either GameErr Game) GameMsgOut IO ()
--gameMsgOutHandler' = undefined



-- (~>) :: Monad m
-- => (a -> Producer b m ())
-- -> (b -> Producer c m ())
-- -> (a -> Producer c m ())
-- 
--   -- do 
--   gameMsgIn <- yield


-- creates a mailbox which has both an input sink and output source which
-- models the bidirectionality of websockets.
-- We return input source which emits our received socket msgs.
websocketInMailbox :: WS.Connection -> MsgHandlerConfig -> IO (Input MsgIn)
websocketInMailbox conn conf@MsgHandlerConfig{..} = do
  (writeMsgInSource, readMsgInSource) <- spawn Unbounded
  (writeMsgOutSource, readMsgOutSource) <- spawn Unbounded
  async $ socketMsgInWriter conn writeMsgInSource -- read parsed MsgIn's from socket and place in incoming mailbox
  async $ (runEffect $ fromInput readMsgInSource >-> msgInHandler conf >-> toOutput writeMsgOutSource) -- process received MsgIn's and place resulting MsgOut in outgoing mailbox
  async $ socketMsgOutWriter conn readMsgOutSource -- send encoded MsgOuts from outgoing mailbox to socket
  --async $ runEffect $ for (fromInput readMsgOutSource >-> logMsgOut) (lift . WS.sendTextData newConn . encodeMsgOutToJSON) -- send MsgOut's waiting in mailbox through socket
 -- async $ runEffect $ fromInput inputMailbox >-> msgInHandler >->

  return readMsgInSource
    -- encode MsgOut values to JSON bytestring to send to socket

-- Runs an IO action forever which parses read MsgIn's from the websocket connection 
-- and puts them in our mailbox waiting to be processed by our MsgIn handler
-- 
--  Note - only parsed MsgIns make it into the mailbox - socket msgs which cannot be parsed
-- are silently ignored but logged anyway.
socketMsgInWriter :: WS.Connection -> Output MsgIn -> IO (Async ())
socketMsgInWriter conn writeMsgInSource = 
    forever $ do
      a <- async $ runEffect $ msgInDecoder (socketReader conn >-> logMsgIn) >-> toOutput writeMsgInSource
      link a

socketMsgOutWriter :: WS.Connection -> Input MsgOut -> IO (Async ())
socketMsgOutWriter conn is =  
  forever $ do
    runEffect $ for (fromInput is >-> msgOutEncoder) (lift . WS.sendTextData conn )


encodeMsgOutToJSON :: MsgOut -> Text
encodeMsgOutToJSON msgOut = X.toStrict $ D.decodeUtf8 $ A.encode msgOut

-- Converts a websocket connection into a producer 
socketReader :: WS.Connection -> Producer BS.ByteString IO ()
socketReader conn = forever $ do
    msg <- liftIO $ WS.receiveData conn
    liftIO $ putStrLn $ "received a msg from socket: " ++ show msg
    yield msg

-- Convert a raw Bytestring producer of raw JSON into a new producer which yields 
-- only successfully parsed values of type MsgIn.
--
-- Note that this parser deliberately ignores parsing errors as the naive implementation
-- would lead to parse errors closing the stream pipeline and thus the socket connection
msgInDecoder :: Producer BS.ByteString IO () -> Producer MsgIn IO ()
msgInDecoder rawMsgProducer = do
  (x, p') <- lift $ runStateT decode rawMsgProducer
  case x of
    Nothing -> return ()
    Just (Left a) -> do
      (x, p'') <- lift $ runStateT draw p'
      lift $ print x
      -- x is the problem input msg which failed to parse. We ignore it here by just resuming
      msgInDecoder p''
    Just c@(Right msgIn) -> do -- successful parsing case
      lift $ print c 
      yield msgIn
      msgInDecoder p'

     
msgOutEncoder :: Pipe MsgOut BS.ByteString IO ()
msgOutEncoder = forever $ do 
  msgOut <- await
  lift $ print "encoding msg: "
  lift $ print msgOut
  yield $ fromString $ T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode msgOut


msgInHandler :: MsgHandlerConfig -> Pipe MsgIn MsgOut IO ()
msgInHandler conf = forever $ do
    msgIn <- await
    liftIO $ print "msghandler : "
    liftIO $ print msgIn
    msgOut <- lift $ runExceptT $ runReaderT (msgHandler msgIn) conf
    let msgOut' = either ErrMsg id msgOut
    yield msgOut'
    return ()
  where 
    sampleMsg = GameMsgOut PlayerLeft


logMsgIn :: Pipe BS.ByteString BS.ByteString IO ()
logMsgIn = do 
  msg <- await
  lift $ putStrLn "logging MsgIn"
  x   <- lift $ try $ print msg
  case x of
      -- Gracefully terminate if we got a broken pipe error
      Left e@(G.IOError { G.ioe_type = t}) ->
          lift $ unless (t == G.ResourceVanished) $ throwIO e
      -- Otherwise loop
      Right () -> yield msg >> logMsgIn


logMsgOut :: Pipe MsgOut MsgOut IO ()
logMsgOut = do 
  msg <- await
  lift $ putStrLn "logging MsgOut"
  x   <- lift $ try $ print msg
  case x of
      -- Gracefully terminate if we got a broken pipe error
      Left e@(G.IOError { G.ioe_type = t}) ->
          lift $ unless (t == G.ResourceVanished) $ throwIO e
      -- Otherwise loop
      Right () -> yield msg >> logMsgOut
      
      

-- tables are stream abstractions which take in game msgs and yield game states
--data Game = Producer GameMsgIn (Either GameErr Game) IO ()

--newtype Table' = Table' (Pipe PlayerAction gameMove IO Game)

-- get a pipe which only forwards the game moves which occur at the given table
filterMsgsForTable tableName = P.filter $ \(GameMove tableName' _) -> tableName == tableName'


-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client.
--
-- After the client has been authenticated we fork a thread which writes
-- the clients msgs to a channel.
application
  :: BS.ByteString
  -> ConnectionString
  -> RedisConfig
  -> TVar ServerState
  -> WS.ServerApp
application secretKey dbConnString redisConfig serverState pending = do
  newConn <- WS.acceptRequest pending
  WS.forkPingThread newConn 30
  msg <- WS.receiveData newConn
  ServerState {..} <- readTVarIO serverState
  
  async $ websocketInMailbox newConn (msgConf newConn)
  authClient secretKey
             serverState
             dbConnString
             redisConfig
             newConn
        --     authenticatedMsgLoop
             (Token msg)
  forever (WS.receiveData newConn :: IO Text) 
 where
  msgConf c = MsgHandlerConfig
    { serverStateTVar = serverState
    , dbConn          = dbConnString
    , clientConn      = c
    , redisConfig     = redisConfig
    , ..
    }

    -------- bots


msgHandler' :: MsgHandlerConfig -> MsgIn -> IO MsgOut
msgHandler' _ _ = return $ GameMsgOut PlayerLeft


delayThenSeatPlayer
  :: ConnectionString -> Int -> TVar ServerState -> Player -> IO ()
delayThenSeatPlayer dbConn delayDuration s p = do
  print "delaying before sit down bot ... "
  _ <- threadDelay delayDuration
  print "about to sit down bot ... "
  sitDownBot dbConn p s
  print "... done . bot sat down "


bot1 :: Player
bot1 = initPlayer "1@1" 2000

bot2 :: Player
bot2 = initPlayer "2@2" 2000

bot3 :: Player
bot3 = initPlayer "3@3" 2000

bot4 :: Player
bot4 = initPlayer "101@101" 2000

bot5 :: Player
bot5 = initPlayer "102@102" 2000

--    dupTableChanMsg <- atomically $ readTChan dupTableChan

startBotActionLoops
  :: ConnectionString -> TVar ServerState -> Int -> [PlayerName] -> IO ()
startBotActionLoops db s playersToWaitFor botNames = do
--  threadDelay 1180000 --delay so bots dont start game until all of them sat down
  ServerState {..} <- readTVarIO s
  case M.lookup tableName $ unLobby lobby of
    Nothing -> error "TableDoesNotExist "
    Just table@Table {..} ->
      mapM_ (botActionLoop db s channel playersToWaitFor) botNames
  where tableName = "Black"


botActionLoop
  :: ConnectionString
  -> TVar ServerState
  -> TChan MsgOut
  -> Int
  -> PlayerName
  -> IO ThreadId
botActionLoop dbConn s tableChan playersToWaitFor botName = forkIO $ do
  chan <- atomically $ dupTChan tableChan
  print botName
  print "create action loop"
  forever $ do
    msg <- atomically $ readTChan chan
    print "action received"
    case msg of
      GameMsgOut (NewGameState tableName g) ->
        unless (shouldn'tStartGameYet g) (actIfNeeded g botName)
      _ -> return ()
 where
  shouldn'tStartGameYet Game {..} =
    (_street == PreDeal && ((length $ _players) < playersToWaitFor))
  actIfNeeded g' pName' =
    let hasToAct = doesPlayerHaveToAct pName' g'
    in  when (hasToAct || (blindRequiredByPlayer g' pName' /= NoBlind)) $ do
          print hasToAct
          runBotAction dbConn s g' pName'


runBotAction
  :: ConnectionString -> TVar ServerState -> Game -> PlayerName -> IO ()
runBotAction dbConn serverStateTVar g pName = do
  maybeAction <- getValidAction g pName
  print g
  print ("Random action from " <> show pName <> " is " <> show maybeAction)
  case maybeAction of
    Nothing -> return ()
    Just a  -> do
      eitherNewGame <- runPlayerAction g a
      case eitherNewGame of
        Left  gameErr -> print (show $ GameErr gameErr) >> return ()
        Right newGame -> do
          atomically $ updateGameAndBroadcastT serverStateTVar tableName newGame
          progressGame' dbConn serverStateTVar tableName newGame
 where
  tableName  = "Black"
  chipsToSit = 2000

sitDownBot :: ConnectionString -> Player -> TVar ServerState -> IO ()
sitDownBot dbConn player@Player {..} serverStateTVar = do
  s@ServerState {..} <- readTVarIO serverStateTVar
  let gameMove = SitDown player
  case M.lookup tableName $ unLobby lobby of
    Nothing         -> error "table doesnt exist" >> return ()
    Just Table {..} -> do
      eitherNewGame <- liftIO $ runPlayerAction game takeSeatAction
      case eitherNewGame of
        Left  gameErr -> print $ GameErr gameErr
        Right newGame -> do
          dbDepositChipsIntoPlay dbConn _playerName chipsToSit
          atomically $ updateGameAndBroadcastT serverStateTVar tableName newGame
 where
  chipsToSit     = 2000
  tableName      = "Black"
  takeSeatAction = PlayerAction { name=_playerName, action = SitDown player}

--runBotAction :: TVar ServerState -> TableName -> Game -> PlayerAction -> STM ()
--runBotAction serverS tableName g botAction = do


actions :: Street -> Int -> [Action]
actions st chips | st == PreDeal = [PostBlind Big, PostBlind Small]
                 | otherwise     = [Check, Call, Fold, Bet chips, Raise chips]


getValidAction :: Game -> PlayerName -> IO (Maybe PlayerAction)
getValidAction g@Game {..} name
  | length _players < 2 = return Nothing
  | _street == PreDeal = return $ case blindRequiredByPlayer g name of
    Small   -> Just $ PlayerAction { action = PostBlind Small, ..}
    Big     -> Just $ PlayerAction { action = PostBlind Big , ..}
    NoBlind -> Nothing
  | otherwise = do
    betAmount' <- randomRIO (lowerBetBound, chipCount)
    let possibleActions  = (actions _street betAmount')
    let actionsValidated = validateAction g name <$> possibleActions
    let actionPairs      = zip possibleActions actionsValidated
    print actionPairs
    let validActions = (<$>) fst $ filter (isRight . snd) actionPairs
    print validActions
    if null validActions then panic else return ()

    randIx <- randomRIO (0, length validActions - 1)

    return $ Just $ PlayerAction {action =validActions !! randIx, ..}
 where
  lowerBetBound = if (_maxBet > 0) then (2 * _maxBet) else _bigBlind
  chipCount     = fromMaybe 0 ((^. chips) <$> (getGamePlayer g name))
  panic         = do
    print $ "UHOH no valid actions for " <> show name
    print g
    error $ "UHOH no valid actions"
