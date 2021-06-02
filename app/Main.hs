module Main where

import Lib

main :: IO ()
main = someFunc

myFunc :: Num a => a -> a
myFunc x = x + 1