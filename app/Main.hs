module Main where

import Data.Char
import Data.Maybe
  
import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL
import qualified Data.Game.PlayingCards.Table as TB
import qualified Data.Game.PlayingCards.Rules.BlackJack as BJ

import System.IO

------------------------------------------------------------------------
initTbl
  :: Int    -- ^ seed of random generator
  -> IO TB.Table
initTbl seed = do
  putStrLn "Initialze Table"
  tbl <- return $ TB.initialTable seed
  v   <- return $ TB.viewTable "Player" tbl
  putStrLn $ show $ TB.player v
  return tbl

------------------------------------------------------------------------
playerDraw
  :: TB.Table
  -> IO TB.Table
playerDraw t = do
  putStrLn $ n ++ " Hand: " ++ (show $ PL.hand p)
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
      (cs, t') <- return $ TB.drawCard n t
      putStrLn $ n ++ " Draw " ++ (show cs)
      playerDraw t'
    else
      return t
  where
    n = "Player"
    p = fromJust $ TB.findPlayer n t
    s = BJ.checkScore p

------------------------------------------------------------------------
dealerDraw
  :: TB.Table
  -> IO TB.Table
dealerDraw t = do
  putStrLn $ n ++ " Hand: " ++ (show $ PL.hand p)
  putStrLn $ n ++ " Score: " ++ (show s)
  if (21 < s)
  then do
    putStrLn $ n ++ " Burst."
    return t
  else if (17 > s)
  then do
    (cs, t') <- return $ TB.drawCard n t
    putStrLn $ n ++ " Draw " ++ (show cs)
    dealerDraw t'
  else
    return t
  where
    n = "Dealer"
    p = fromJust $ TB.findPlayer n t
    s = BJ.checkScore p

------------------------------------------------------------------------
main :: IO ()
main = do
  t <- initTbl 0
  t' <- playerDraw t
  t'' <- dealerDraw t'
  putStrLn $ "Winner : " ++ (PL.name $ head $ BJ.getWinner t'')
