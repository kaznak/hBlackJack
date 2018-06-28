
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

newtype Dollars = Dollars Int
  deriving(Show, Num)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

