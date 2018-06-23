
module Data.Game.PlayingCards.Table
    ( Table(..)

    , makeTable

    , clearDeck
    , newDeck
    , flipDeck
    , shuffleDeck

    , clearGrave
    , reviveGrave

    , findPlayer
    , findPlayerSeat
    , addPlayer

    , drawCard
    , drawCardFront

    , viewTable

    , initialTable -- composit function
    ) where

import Data.List
import Data.Maybe

import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL

import System.Random

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
clearDeck (Table rg _ g p) = (Table rg [] g p)

------------------------------------------------------------------------
newDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
newDeck (Table rg _ g p) = (Table rg d g p)
  where
    d :: [PC.Card]
    d = PC.makeDeck 0

------------------------------------------------------------------------
flipDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
flipDeck (Table rg d g p) = (Table rg (PC.flipDeck d) g p)

------------------------------------------------------------------------
rseq :: RandomGen gen => Int -> gen -> ([Int],[gen])
rseq n = unzip . rseq' (n - 1)

------------------------------------------------------------------------
rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
rseq' 0 _ = []
rseq' i rg = (j, rg) : rseq' (i - 1) rg'
  where
    (j, rg') = randomR (0, i) rg

------------------------------------------------------------------------
shuffleDeck
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
shuffleDeck (Table rg d g p) = (Table rg' d' g p)
  where
    d' = PC.shuffleDeck d r

    r = fst result
    rg' = last $ snd result

    result = rseq (length d) rg

------------------------------------------------------------------------
clearGrave
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
clearGrave (Table rg d _ p) = (Table rg d [] p)

------------------------------------------------------------------------
reviveGrave
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
reviveGrave (Table rg d g p) = (Table rg (g ++ d) [] p)

------------------------------------------------------------------------
findPlayerSeat
  :: String    -- ^ player name
  -> Table     -- ^ table
  -> Maybe Int -- ^ seat number
findPlayerSeat n (Table _ _ _ ps) = findIndex (isName n) ps
  where
    isName _ Nothing = False
    isName name (Just (PL.Player n' _)) = name == n'

------------------------------------------------------------------------
findPlayer
  :: String          -- ^ player name
  -> Table           -- ^ table
  -> Maybe PL.Player -- ^ player
findPlayer n (Table _ _ _ ps) =
  find ((n==).PL.name) $ map fromJust $ filter isJust ps

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
addPlayer _ sn t               | not $ isEmptySeat sn t      = error "seat is occupied."
addPlayer (PL.Player n _) _ t  | isJust $ findPlayerSeat n t = error $ "player " ++ n ++ " already on seat."
addPlayer p sn t               | otherwise                   = updateSeat p sn t

------------------------------------------------------------------------
updateSeat
  :: PL.Player -- ^ Player
  -> Int       -- ^ Seat Number
  -> Table     -- ^ Previous table state
  -> Table     -- ^ New table state
updateSeat p sn (Table rg d g ps) = Table rg d g ps'
  where
    ps'  = left ++ [Just p] ++ right
    left = take sn ps
    right = drop (sn+1) ps

------------------------------------------------------------------------
drawCard'
  :: (PC.Card -> PC.Card) -- ^ Action for card
  -> String               -- ^ Player name
  -> Table                -- ^ Previous table state
  -> ([PC.Card], Table)     -- ^ Card and New table state
drawCard' act n t@(Table rg d g ps) =
  if isNothing mi
  then error $ "player " ++ n ++ " is not seat."
  else (cs', Table rg d' g ps')
  where
    mi = findPlayerSeat n t
    sn = fromJust mi
    
    (cs, d') = PC.drawCards d 1
    cs' = map act cs

    p' = PL.addHand cs' (fromJust $ ps!!sn)
    (Table _ _ _ ps') = updateSeat p' sn t

------------------------------------------------------------------------
drawCard
  :: String               -- ^ Player name
  -> Table                -- ^ Previous table state
  -> ([PC.Card], Table)     -- ^ Card and New table state
drawCard = drawCard' id

------------------------------------------------------------------------
drawCardFront
  :: String               -- ^ Player name
  -> Table                -- ^ Previous table state
  -> ([PC.Card], Table)     -- ^ Card and New table state
drawCardFront = drawCard' PC.frontCard

------------------------------------------------------------------------
viewTable
  :: String   -- ^ Player name
  -> Table -- ^ Actual Table
  -> Table -- ^ Players View Table
viewTable n t | isNothing $ findPlayerSeat n t =
                  error $ "player " ++ n ++ " does not seat."
viewTable n (Table rg d g ps) | otherwise = Table rg d' g' ps'
  where
    d' = map PC.viewCard d
    g' = map PC.viewCard g
    ps' = map viewSeat ps

    viewSeat Nothing  = Nothing
    viewSeat s@(Just (PL.Player n' _)) | n' == n = s
    viewSeat s | otherwise = fmap PL.viewPlayer s

------------------------------------------------------------------------
------------------------------------------------------------------------
initialTable -- composit function
  :: Int   -- ^ seed of random generator
  -> Table
initialTable seed =
  snd $ drawCard "Dealer" $ snd $ drawCardFront "Dealer" $ 
  snd $ drawCard "Player" $ snd $ drawCardFront "Player" $ 
  shuffleDeck $ flipDeck    $ newDeck $
  clearDeck   $ clearGrave  $
  addPlayer (PL.makePlayer "Player") 1 $
  addPlayer (PL.makePlayer "Dealer") 0 $
  makeTable seed 2
