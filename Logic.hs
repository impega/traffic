{-# LANGUAGE TupleSections #-}

module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Maybe
import Block

type State = Maybe Side

step :: State -> Board -> (State, Board)
step st = (st,) . maybe id safeMove st

react :: Event -> State -> Board -> (State, Board)
react (EventKey (SpecialKey _) Up _ _) _ = (Nothing,)
react (EventKey (SpecialKey KeyTab) Down _ _) st = (st,) . nextBlock
react (EventKey (SpecialKey dir) Down _ _)    st =
  maybe (st,) (\ d -> (Just d,) . safeMove d) (direction dir)
react _ st = (st,)

nextBlock :: Board -> Board
nextBlock board = board { content = bs ++ [ b ] }
  where (b : bs) = content board

direction :: SpecialKey -> Maybe Side
direction KeyLeft  = Just West
direction KeyRight = Just East
direction KeyUp    = Just North
direction KeyDown  = Just South
direction _        = Nothing

corners :: Block -> [Position]
corners b =
  let botLeft  = blockPosition b
      botRight = botLeft  { x_pos = x_pos botLeft  + blockWidth b }
      topLeft  = botLeft  { y_pos = y_pos botLeft  + blockHeight b }
      topRight = botRight { y_pos = y_pos botRight + blockHeight b }
  in [ botLeft, botRight, topLeft, topRight ]

isWestOf :: Position -> [Position] -> Bool
isWestOf p = all (\ q -> x_pos p <= x_pos q)

isEastOf :: Position -> [Position] -> Bool
isEastOf p = all (\ q -> x_pos q <= x_pos p)

isNorthOf :: Position -> [Position] -> Bool
isNorthOf p = all (\ q -> y_pos q <= y_pos p)

isSouthOf :: Position -> [Position] -> Bool
isSouthOf p = all (\ q -> y_pos p <= y_pos q)

areOutsideOf :: [Position] -> Block -> Bool
areOutsideOf ps b =
  let botLeft  = blockPosition b in
  let topRight = Position { x_pos = x_pos botLeft + blockWidth b
                          , y_pos = y_pos botLeft + blockHeight b}
  in botLeft  `isEastOf`  ps
  || botLeft  `isNorthOf` ps
  || topRight `isWestOf`  ps
  || topRight `isSouthOf` ps

detectCrash :: Board -> Maybe Board
detectCrash board =
  let (b : bs) = content board   in
  let pos_b    = blockPosition b in
  guard (0 <= x_pos pos_b)
  >> guard (x_pos pos_b + blockWidth b <= boardWidth board)
  >> guard (0 <= y_pos pos_b)
  >> guard (y_pos pos_b + blockHeight b <= boardHeight board)
  >> guard (all (corners b `areOutsideOf`) bs)
  >> return board

move :: Side -> Board -> Maybe Board
move side board =
  let (b : bs) = content board in
  if blockWidth b > blockHeight b
  then case side of
         West -> Just $ board { content = move_x b (-1) : bs }
         East -> Just $ board { content = move_x b 1    : bs }
         _    -> Nothing
  else case side of
         North -> Just $ board { content = move_y b 1    : bs }
         South -> Just $ board { content = move_y b (-1) : bs }
         _     -> Nothing

safeMove :: Side -> Board -> Board
safeMove side board = fromMaybe board $ detectCrash =<< move side board
