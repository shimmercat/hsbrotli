{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Compression.Brotli
import Data.ByteString.Lazy as LB


--someContent :: LB.ByteString
--someContent = "Hello world, there is probably more to write in a file"


main :: IO ()
main = do
    someContent <- LB.getContents
    let
        compressed = compress someContent
    LB.writeFile "compressed.br" compressed
