
module BlackJack
  where

import Data.List
import Data.Maybe
import Data.Char
import Data.Maybe

import System.Random
import System.Random.Shuffle

import System.IO

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | Card type definition.

data Suit = Club | Diamond | Heart | Spade
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Value = Ace  | Two   | Three | Four | Five
           | Six  | Seven | Eight | Nine | Ten
           | Jack | Queen | King
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Side = Back | Front
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | BackCard is other players hand and used to represent unknown card.
data Card = Card
  { suit  :: Suit
  , value :: Value
  , side  :: Side
  } | Jorker
  { side :: Side
  } | BackCard
  deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------
makeDeck
  :: Int  -- ^ number of jorkers
  -> [Card]
makeDeck nj = cards ++ jorkers
  where jorkers = take nj $ repeat $ Jorker Front
        cards = [Card s v Front|s <- [Club .. Spade], v <- [Ace .. King]]

------------------------------------------------------------------------
flipCard
  :: Card
  -> Card
flipCard (Jorker Front)   = Jorker Back
flipCard (Jorker Back)    = Jorker Front
flipCard (Card s v Front) = Card s v Back
flipCard (Card s v Back)  = Card s v Front
flipCard BackCard         = error "try to flip BackCard."

------------------------------------------------------------------------
frontCard
  :: Card
  -> Card
frontCard (Jorker _)    = Jorker Front
frontCard (Card s v _)  = Card s v Front
frontCard BackCard      = error "try to flip BackCard."

------------------------------------------------------------------------
viewCard
  :: Card
  -> Card
viewCard (Jorker Front)   = Jorker Back
viewCard (Jorker Back)    = BackCard
viewCard (Card s v Front) = Card s v Back
viewCard (Card _ _ Back)  = BackCard
viewCard BackCard         = BackCard

------------------------------------------------------------------------
flipDeck
  :: [Card]
  -> [Card]
flipDeck deck = reverse $ map flipCard deck

------------------------------------------------------------------------
shuffleDeck
  :: [Card] -- ^ Deck (c1, c2 .. cn)
  -> [Int]  -- ^ Sample Number (r1, r2 .. r(n-1))
  -> [Card]    
shuffleDeck deck r = shuffle deck r

------------------------------------------------------------------------
drawCards
  :: [Card]           -- ^ Deck
  -> Int              -- ^ Number of cards
  -> ([Card], [Card]) -- ^ Took cards and Rest cards
drawCards d n = (take n d, drop n d)

------------------------------------------------------------------------
------------------------------------------------------------------------
data Player = Player
  { name :: String    -- ^ Name of Player, which work as ID.
  , hand :: [Card] -- ^ Hand of Player.
  } deriving Show

------------------------------------------------------------------------
makePlayer
  :: String -- ^ player name
  -> Player
makePlayer n = Player n []

------------------------------------------------------------------------
addHand
  :: [Card] -- ^ add cards to hand
  -> Player    -- ^ previous player state
  -> Player    -- ^ next player state
addHand cs (Player n h) = Player n (h ++ cs)

------------------------------------------------------------------------
dropHand
  :: Player    -- ^ previous player state
  -> Player    -- ^ next player state
dropHand (Player n _) = Player n []

------------------------------------------------------------------------
flipHand
  :: Int       -- ^ Index of hand that to be flipped
  -> Player    -- ^ previous player state
  -> Player    -- ^ next player state
flipHand ind (Player n h) = Player n h'
  where
    h' = p ++ [(flipCard $ h !! ind)] ++ r
    p  = take ind h
    r  = drop (ind + 1) h

------------------------------------------------------------------------
viewPlayer
  :: Player    -- ^ previous player state
  -> Player    -- ^ next player state
viewPlayer (Player n h) = Player n h'
  where
    h' = map viewCard h

------------------------------------------------------------------------
------------------------------------------------------------------------
-- | A BlackJack Table data type.
data Table = Table
  { gen    :: StdGen
  , deck   :: [Card]
  , grave  :: [Card]   -- ^ Discarded cards.
  , player :: [Maybe Player] -- ^ Players.
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
    d :: [Card]
    d = makeDeck 0

------------------------------------------------------------------------
flipDeckT
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
flipDeckT (Table rg d g p) = (Table rg (flipDeck d) g p)

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
shuffleDeckT
  :: Table -- ^ Previous table state
  -> Table -- ^ New table state
shuffleDeckT (Table rg d g p) = (Table rg' d' g p)
  where
    d' = shuffleDeck d r

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
    isName name (Just (Player n' _)) = name == n'

------------------------------------------------------------------------
findPlayer
  :: String          -- ^ player name
  -> Table           -- ^ table
  -> Maybe Player -- ^ player
findPlayer n (Table _ _ _ ps) =
  find ((n==).name) $ map fromJust $ filter isJust ps

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
  :: Player -- ^ Player
  -> Int       -- ^ Seat Number
  -> Table     -- ^ Previous table state
  -> Table     -- ^ New table state
addPlayer _ sn t           | not $ isEmptySeat sn t      = error "seat is occupied."
addPlayer (Player n _) _ t | isJust $ findPlayerSeat n t = error $ "player " ++ n ++ " already on seat."
addPlayer p sn t           | otherwise                   = updateSeat p sn t

------------------------------------------------------------------------
updateSeat
  :: Player -- ^ Player
  -> Int    -- ^ Seat Number
  -> Table  -- ^ Previous table state
  -> Table  -- ^ New table state
updateSeat p sn (Table rg d g ps) = Table rg d g ps'
  where
    ps'  = left ++ [Just p] ++ right
    left = take sn ps
    right = drop (sn+1) ps

------------------------------------------------------------------------
drawCard'
  :: (Card -> Card)  -- ^ Action for card
  -> String          -- ^ Player name
  -> Table           -- ^ Previous table state
  -> ([Card], Table) -- ^ Card and New table state
drawCard' act n t@(Table rg d g ps) =
  if isNothing mi
  then error $ "player " ++ n ++ " is not seat."
  else (cs', Table rg d' g ps')
  where
    mi = findPlayerSeat n t
    sn = fromJust mi
    
    (cs, d') = drawCards d 1
    cs' = map act cs

    p' = addHand cs' (fromJust $ ps!!sn)
    (Table _ _ _ ps') = updateSeat p' sn t

------------------------------------------------------------------------
drawCard
  :: String          -- ^ Player name
  -> Table           -- ^ Previous table state
  -> ([Card], Table) -- ^ Card and New table state
drawCard = drawCard' id

------------------------------------------------------------------------
drawCardFront
  :: String          -- ^ Player name
  -> Table           -- ^ Previous table state
  -> ([Card], Table) -- ^ Card and New table state
drawCardFront = drawCard' frontCard

------------------------------------------------------------------------
viewTable
  :: String -- ^ Player name
  -> Table  -- ^ Actual Table
  -> Table  -- ^ Players View Table
viewTable n t | isNothing $ findPlayerSeat n t =
                  error $ "player " ++ n ++ " does not seat."
viewTable n (Table rg d g ps) | otherwise = Table rg d' g' ps'
  where
    d' = map viewCard d
    g' = map viewCard g
    ps' = map viewSeat ps

    viewSeat Nothing  = Nothing
    viewSeat s@(Just (Player n' _)) | n' == n = s
    viewSeat s | otherwise = fmap viewPlayer s

------------------------------------------------------------------------
------------------------------------------------------------------------
initialTable -- composit function
  :: Int     -- ^ seed of random generator
  -> Table
initialTable seed =
  snd $ drawCard "Dealer" $ snd $ drawCardFront "Dealer" $ 
  snd $ drawCard "Player" $ snd $ drawCardFront "Player" $ 
  shuffleDeckT $ flipDeckT  $ newDeck $
  clearDeck    $ clearGrave $
  addPlayer (makePlayer "Player") 1 $
  addPlayer (makePlayer "Dealer") 0 $
  makeTable seed 2

------------------------------------------------------------------------
------------------------------------------------------------------------
data GameState = GameState
  { winner :: [Player]
  } deriving(Show)

------------------------------------------------------------------------
cardScore
  :: Card
  -> Int
cardScore (Card _ v _) | s < 10 = s + 1
  where s = fromEnum v
cardScore (Card _ _ _) = 10
cardScore _  = error "bad card."

------------------------------------------------------------------------
checkScore
  :: Player -- ^ player state
  -> Int
checkScore = sum . map cardScore . hand

------------------------------------------------------------------------
getWinner
  :: Table -- ^ table
  -> [Player]
getWinner (Table _ _ _ ps) =
  if (psc > 21)
  then [d]
  else if dsc > 21
  then [p]
  else if (dsc > psc)
  then [d]
  else [p]
  where
    d = fromJust $ ps !! 0
    dsc = checkScore d
    p = fromJust $ ps !! 1
    psc = checkScore p

------------------------------------------------------------------------
------------------------------------------------------------------------
initTbl
  :: Int    -- ^ seed of random generator
  -> IO Table
initTbl seed = do
  putStrLn "Initialze Table"
  tbl <- return $ initialTable seed
  v   <- return $ viewTable "Player" tbl
  putStrLn $ show $ player v
  return tbl

------------------------------------------------------------------------
playerDraw
  :: Table
  -> IO Table
playerDraw t = do
  putStrLn $ n ++ " Hand: " ++ (show $ hand p)
  putStrLn $ n ++ " Score: " ++ (show s)
  if (21 < s)
  then do
    putStrLn $ n ++ " Burst."
    return t
  else do
    putStrLn $ n ++ " Call?[Y/]"
    c <- fmap (toUpper . head) getLine
    if 'Y' == c
    then do
      (cs, t') <- return $ drawCard n t
      putStrLn $ n ++ " Draw " ++ (show cs)
      playerDraw t'
    else
      return t
  where
    n = "Player"
    p = fromJust $ findPlayer n t
    s = checkScore p

------------------------------------------------------------------------
dealerDraw
  :: Table
  -> IO Table
dealerDraw t = do
  putStrLn $ n ++ " Hand: " ++ (show $ hand p)
  putStrLn $ n ++ " Score: " ++ (show s)
  if (21 < s)
  then do
    putStrLn $ n ++ " Burst."
    return t
  else if (17 > s)
  then do
    (cs, t') <- return $ drawCard n t
    putStrLn $ n ++ " Draw " ++ (show cs)
    dealerDraw t'
  else
    return t
  where
    n = "Dealer"
    p = fromJust $ findPlayer n t
    s = checkScore p

------------------------------------------------------------------------
main :: IO ()
main = do
  t <- initTbl 0
  t' <- playerDraw t
  t'' <- dealerDraw t'
  putStrLn $ "Winner : " ++ (name $ head $ getWinner t'')
