module Main where

import Codec.Compression.Brotli
import Data.ByteString.Lazy as LB

main :: IO ()
main = do
    stuff <- LB.getContents
    LB.putStr $ compress stuff
