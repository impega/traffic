module Main where

import System.Environment
import Graphics.Gloss
import Block
import Logic

main :: IO ()
main = do
  (m : n : _) <- fmap (fmap read) getArgs
  let k = 20
  let window = InWindow "Nice Window" (k * (2 + m), k * (2 + n)) (210, 210)
  let world  = Board m n (Door 2 3 West) $
                [ Block 5 2  red  (Position 2 3)
                , Block 1 4  blue (Position 15 1)
                , Block 2 1  green (Position 25 1) ]
  play window black 24 world (displayBoard k) react (const id)
