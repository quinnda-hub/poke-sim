module Main where

import qualified MyLib (someDemo)

main :: IO ()
main = do
  MyLib.someDemo
