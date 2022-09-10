module Main where

import Config
import Lib

main :: IO ()
main = readConfig >>= play
