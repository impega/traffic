module Main where

import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Block
import Logic

main :: IO ()
main = do
  let k = 100
  let m = 6
  let n = 6
  let window = InWindow "Nice Window" (k * (2 + m), k * (2 + n)) (210, 210)
  let brown  = makeColorI 139 80 14 255
  let purple = makeColorI 128 0 128 255
  let pink   = mixColors 0.5 0.5 rose purple
  let orange = mixColors 0.5 0.5 yellow red
  let world  = Board m n (Door 1 3 East) $
                [ Block 2 1 green        (Position 0 5)
                , Block 1 2 rose         (Position 0 3)
                , Block 2 1 (greyN 0.5)  (Position 0 1)
                , Block 1 2 brown        (Position 2 0)
                , Block 2 1 orange       (Position 3 0)
                , Block 2 1 yellow       (Position 4 1)
                , Block 1 2 purple       (Position 5 2)
                , Block 2 1 (dark green) (Position 3 2)
                , Block 1 2 pink         (Position 3 4)
                , Block 1 2 (dark blue)  (Position 5 4)
                , Block 1 3 yellow       (Position 4 3)
                , Block 2 1 white        (Position 2 3)
                ]
  play window black 40
    (Just (Nothing, world))
    (maybe (Color white $ Text "You Won!") (displayBoard k . snd))
    (\ e s -> s >>= fmap uncurry (react k) e)
    (const (uncurry step =<<))
