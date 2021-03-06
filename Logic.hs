{-# LANGUAGE TupleSections #-}

module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Maybe
import Block

type State = Maybe (Mode, Side)
newtype Mode = Mode { isFast :: Bool }

fast :: Mode
fast = Mode { isFast = True }

slow :: Mode
slow = Mode { isFast = False }

step :: State -> Board -> Maybe (State, Board)
step st = fmap (st,) . maybe return (uncurry stepMS) st
   where
     stepMS :: Mode -> Side -> Board -> Maybe Board
     stepMS m s = if isFast m then safeMove s else return

winning :: Side -> Board -> Bool
winning d board =
  let (b : bs) = content board
      bl       = botLeft b
      tr       = topRight b
      target   = door board
      begin    = doorDistance target
      finish   = (begin, begin + doorSize target)
  in
  let ((ltx, gtx), (lty, gty)) =
        case doorSide target of
          South -> (finish, (0, blockHeight b))
          North -> (finish, (boardHeight board - blockHeight b, boardHeight board))
          West  -> ((0, blockWidth b), finish)
          East  -> ((boardWidth board - blockWidth b, boardWidth board), finish)
  in white == blockColour b
  && d == doorSide target
  && ltx <= x_pos bl && x_pos tr <= gtx
  && lty <= y_pos bl && y_pos tr <= gty

aux :: Int -> Int -> [Block] -> [Block] -> [Block]
aux x y acc []       = reverse acc
aux x y acc (b : bs) =
      let bl = botLeft b
          tr = topRight b
      in if x_pos bl <= x && x < x_pos tr
         && y_pos bl <= y && y < y_pos tr
         then b : bs ++ acc
         else aux x y (b : acc) bs

select :: Int -> Int -> Board -> Board
select x y board = board { content = aux x y [] (content board) }

react :: Int -> Event -> State -> Board -> Maybe (State, Board)
react k (EventKey (MouseButton LeftButton) Down _ (x, y)) st board =
  return $ (st,) . select (scaleX x) (scaleY y) $ board
  where scaleX x = boardWidth board `quot` 2 + floor (x / fromIntegral k)
        scaleY y = boardHeight board `quot` 2 + floor (y / fromIntegral k)
react _ (EventKey (SpecialKey dir) Up _ _) _ board
  | isJust (direction dir) = return (Nothing, board)
react _ (EventKey (SpecialKey k) Up _ _) st board
  | k `elem` [KeyShiftL, KeyShiftR] = return . (fmap ((fast,) . snd) st,) $ board
react _ (EventKey (SpecialKey k) Down _ _) st board
  | k `elem` [KeyShiftL, KeyShiftR] = return . (fmap ((slow,) . snd) st,) $ board
react _ (EventKey (SpecialKey KeyTab) Down _ _) st board = return . (st,) . nextBlock $ board
react _ (EventKey (SpecialKey dir) Down mods _) st board = maybe (return . (st,)) valid (direction dir) $ board
  where valid d = fmap (Just (Mode (shift mods == Up), d),) . safeMove d
react _ _ st board = return (st, board)

nextBlock :: Board -> Board
nextBlock board = board { content = bs ++ [ b ] }
  where (b : bs) = content board

direction :: SpecialKey -> Maybe Side
direction KeyLeft  = Just West
direction KeyRight = Just East
direction KeyUp    = Just North
direction KeyDown  = Just South
direction _        = Nothing


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
  let bl = botLeft b
      tr = topRight b
  in bl  `isEastOf`  ps
  || bl  `isNorthOf` ps
  || tr `isWestOf`  ps
  || tr `isSouthOf` ps

detectCrash :: Board -> Maybe Board
detectCrash board =
  let (b : bs) = content board
      bl       = botLeft b
      tr       = topRight b in
  guard (
     -- the block is still inside the grid
        0 <= x_pos bl
     && 0 <= y_pos bl
     && x_pos tr <= boardWidth board
     && y_pos tr <= boardHeight board
     -- it does not intersect with any other block
     && all (corners b `areOutsideOf`) bs
  ) >> return board

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

safeMove :: Side -> Board -> Maybe Board
safeMove side board =
  guard (not $ winning side board)
  >> return (fromMaybe board $ detectCrash =<< move side board)
