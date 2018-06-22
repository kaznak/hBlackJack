
-- | Simple BlackJack

module Data.Game.PlayingCards.Table
    ( Table(..)

    , makeTable

    , clearDeck
    , newDeck
    , flipDeck
    , shuffleDeck

    , renewShuffleDeck -- composed action
    
    , clearGrave
    , reviveGrave

    , drawCard

    , addPlayer
    ) where

import Development.Placeholders

import Data.List
import Data.Maybe

import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL

import System.Random
import System.Random.Shuffle

import Control.Monad.State.Lazy

------------------------------------------------------------------------
-- | A BlackJack Table data type.
data Table = Table
  { gen    :: StdGen
  , deck   :: [PC.Card]
  , grave  :: [PC.Card]   -- ^ Discarded cards.
  , player :: [Maybe PL.Player] -- ^ Players.
  } deriving Show

------------------------------------------------------------------------
makeTable
  :: Int   -- ^ seed of random number generator.
  -> Int   -- ^ number of seats. 0 is dealers one.
  -> Table
makeTable seed seats = Table
  { gen    = mkStdGen seed
  , deck   = []
  , grave  = []
  , player = replicate seats Nothing
  }

------------------------------------------------------------------------
clearDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
clearDeck (Table gen _ g p) = (Table gen [] g p)

------------------------------------------------------------------------
newDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
newDeck (Table gen _ g p) = (Table gen d g p)
  where
    d :: [PC.Card]
    d = PC.makeDeck 0

------------------------------------------------------------------------
flipDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
flipDeck (Table gen d g p) = (Table gen (PC.flipDeck d) g p)

------------------------------------------------------------------------
rseq :: RandomGen gen => Int -> gen -> ([Int],[gen])
rseq n = unzip . rseq' (n - 1)

------------------------------------------------------------------------
rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
rseq' 0 _ = []
rseq' i gen = (j, gen) : rseq' (i - 1) gen'
  where
    (j, gen') = randomR (0, i) gen

------------------------------------------------------------------------
shuffleDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
shuffleDeck (Table gen d g p) = (Table gen' d' g p)
  where
    d' = PC.shuffleDeck d r

    r = fst result
    gen' = last $ snd result

    result = rseq (length d) gen

------------------------------------------------------------------------
renewShuffleDeck -- composed action
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
renewShuffleDeck t = shuffleDeck $ flipDeck $ newDeck $ clearDeck t

------------------------------------------------------------------------
clearGrave
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
clearGrave (Table gen d g p) = (Table gen d [] p)

------------------------------------------------------------------------
reviveGrave
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
reviveGrave (Table gen d g p) = (Table gen (g ++ d) [] p)

------------------------------------------------------------------------
findPlayerSeat
  :: String    -- ^ player name
  -> Table     -- ^ table
  -> Maybe Int -- ^ seat number
findPlayerSeat n (Table _ _ _ ps) = findIndex (isName n) ps
  where
    isName _ Nothing = False
    isName name (Just (PL.Player n _)) = name == n

------------------------------------------------------------------------
findEmptySeat
  :: Table -- ^ table
  -> [Int] -- ^ seat numbers
findEmptySeat (Table _ _ _ ps) = findIndices isNothing ps

------------------------------------------------------------------------
isEmptySeat
  :: Int   -- ^ seat number
  -> Table -- ^ table
  -> Bool  -- ^ seat numbers
isEmptySeat sn (Table _ _ _ ps) = isNothing $ ps !! sn

------------------------------------------------------------------------
addPlayer
  :: PL.Player -- ^ Player
  -> Int       -- ^ Seat Number
  -> Table     -- ^ Previous table state
  -> Table     -- ^ New table state
addPlayer _ sn t                  | not $ isEmptySeat sn t      = error "seat is occupied."
addPlayer (PL.Player n _) _ t     | isJust $ findPlayerSeat n t = error $ "player " ++ n ++ " already on seat."
addPlayer p sn (Table gen d g ps) | otherwise                   = Table gen d g ps'
  where
    ps'  = left ++ [Just p] ++ right
    left = take sn ps
    right = drop (sn+1) ps

------------------------------------------------------------------------
drawCard
  :: String -- ^ Player name
  -> Table  -- ^ Previous table state
  -> Table  -- ^ New table state
drawCard n t@(Table gen d g ps) =
  if isNothing mi
  then error $ "player " ++ n ++ " is not seat."
  else Table gen d' g ps'
  where
    mi = findPlayerSeat n t
    sn = fromJust mi

    (c, d') = PC.drawCard d 1
    ps' = left ++ [Just p'] ++ right
    left = take sn ps
    right = drop (sn+1) ps

    Just (PL.Player name hand) = ps !! sn    
    p' = PL.Player name $ c ++ hand

------------------------------------------------------------------------
drawFrontCard
  :: String -- ^ Player name
  -> Table  -- ^ Previous table state
  -> Table  -- ^ New table state
drawFrontCard n t@(Table gen d g ps) =
  if isNothing mi
  then error $ "player " ++ n ++ " is not seat."
  else Table gen d' g ps'
  where
    mi = findPlayerSeat n t
    sn = fromJust mi

    ([PC.Card suit value side], d') = PC.drawCard d 1
    c = [PC.Card suit value PC.Front]
    
    ps' = left ++ [Just p'] ++ right
    left = take sn ps
    right = drop (sn+1) ps

    Just (PL.Player name hand) = ps !! sn    
    p' = PL.Player name $ c ++ hand
