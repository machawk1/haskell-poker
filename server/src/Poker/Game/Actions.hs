{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Actions where

import qualified Data.List.Safe as Safe

import Control.Lens
import Control.Monad.State

import Data.Bool
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

import Poker.ActionValidation
import Poker.Game.Blinds
import Poker.Game.Utils
import Poker.Types

import Text.Read (readMaybe)

import Prelude

-- Update table maxBet and pot as well as player state and chip count
placeBet :: Int -> Player -> Player
placeBet value plyr =
  let chips' = plyr ^. chips
      hasEnoughChips = chips' > value
      betAmount = bool chips' value hasEnoughChips
   in plyr &
      (chips -~ betAmount) .
      (bet +~ betAmount) . (committed +~ betAmount) . (playerState .~ In)

markActed :: Player -> Player
markActed = actedThisTurn .~ True

updateMaxBet :: Int -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand :: Player -> Player
markInForHand = playerState .~ In

-- Will increment the game's current position to act to the next position
-- where a blind is required. Skipping players that do not have to post blinds
-- during the PreDeal phase of the game is desirable as by definition 
-- the only possible players actions during the PreDeal phase are to either:
--   1. Sit out of the game
--   2. Post a blind.
postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} =
  game &
  (players .~ newPlayers) .
  (pot +~ blindValue) .
  (currentPosToAct .~ nextRequiredBlindPos) . (maxBet .~ newMaxBet)
  where
    plyrs = unPlayers _players
    newPlayers = Players $
      (\p@Player {..} ->
         if _playerName == pName
           then (markInForHand . markActed . placeBet blindValue) p
           else p) <$>
      (unPlayers _players)
    isFirstBlind = sum ((\Player {..} -> _bet) <$> plyrs) == 0
    gamePlayerNames = (\Player {..} -> _playerName) <$> plyrs
    blindValue =
      if blind == Small
        then _smallBlind
        else _bigBlind
    newMaxBet =
      if blindValue > _maxBet
        then blindValue
        else _maxBet
    positionOfBlindPoster =
      fromJust $ V.findIndex ((== pName) . (^. playerName)) plyrs
    nextRequiredBlindPos = getPosNextBlind positionOfBlindPoster game

makeBet :: Int -> PlayerName -> Game -> Game
makeBet amount pName game@Game {..} =
  updateMaxBet amount game &
  (players .~ newPlayers) . (currentPosToAct .~ nextPosToAct) . (pot +~ amount)
  where
    newPlayers = Players $
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . placeBet amount) p
           else p) <$>
      (unPlayers _players)
    nextPosToAct = incPosToAct _currentPosToAct game

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  game & (players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)
  where
    newPlayers = Players $
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . (playerState .~ Folded)) p
           else p) <$>
      (unPlayers _players)
    nextPosToAct = incPosToAct _currentPosToAct game

call :: PlayerName -> Game -> Game
call pName game@Game {..} =
  game &
  (players .~ newPlayers) .
  (currentPosToAct .~ nextPosToAct) . (pot +~ callAmount)
  where
    player = fromJust $ find (\Player {..} -> _playerName == pName) (unPlayers _players)
    callAmount =
      let maxBetShortfall = _maxBet - (player ^. bet)
          playerChips = (player ^. chips)
       in if maxBetShortfall > playerChips
            then playerChips
            else maxBetShortfall
    newPlayers = Players $
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . placeBet callAmount) p
           else p) <$>
      (unPlayers _players)
    nextPosToAct = incPosToAct _currentPosToAct game

check :: PlayerName -> Game -> Game
check pName game@Game {..} =
  game & (players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)
  where
    newPlayers = Players $
      (\p@Player {..} ->
         if _playerName == pName
           then markActed p
           else p) <$>
      (unPlayers _players)
    nextPosToAct = incPosToAct _currentPosToAct game

-- Sets state of a given player to None (sat-out)
-- In order to sit in again the player must post a blind
sitOut :: PlayerName -> Game -> Game
sitOut plyrName =
  players %~
  Players . (<$>)
    (\p@Player {..} ->
       if _playerName == plyrName
         then Player {_playerState = None, _actedThisTurn = True, ..}
         else p) . unPlayers

sitIn :: PlayerName -> Game -> Game
sitIn plyrName =
  players %~
  Players . (<$>)
    (\p@Player {..} ->
       if _playerName == plyrName
         then Player {_playerState = In, _actedThisTurn = False, ..}
         else p) . unPlayers

seatPlayer :: Player -> Game -> Game
seatPlayer plyr Game{..} = Game{ _players = Players $ plyr `V.cons` (unPlayers _players), ..}

joinWaitlist :: Player -> Game -> Game
joinWaitlist plyr = waitlist %~ (:) (plyr ^. playerName)

leaveSeat :: PlayerName -> Game -> Game
leaveSeat plyrName = players %~ Players . V.filter (\Player {..} -> plyrName /= _playerName) . unPlayers
