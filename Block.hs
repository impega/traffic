{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Block where

import Data.Tuple
import Graphics.Gloss

data Position =
  Position { x_pos :: Int
           , y_pos :: Int }

data Block =
  Block { blockWidth    :: Int
        , blockHeight   :: Int
        , blockColour   :: Color
        , blockPosition :: Position }

data Side = North | West | South | East
  deriving (Eq, Enum)

data Door =
  Door { doorSize     :: Int
       , doorDistance :: Int
       , doorSide     :: Side }

data Board =
  Board { boardWidth  :: Int
        , boardHeight :: Int
        , door        :: Door
        , content     :: [Block]
        }

displayBlock :: Int -> Block -> Picture
displayBlock k Block{..} =
  color blockColour $ polygon $
  [ step 0           0
  , step 0           blockWidth
  , step blockHeight blockWidth
  , step blockHeight 0 ]
  where
    step  x y = (stepx x, stepy y)
    stepx x   = fromIntegral $ k * (x_pos blockPosition + x)
    stepy   y = fromIntegral $ k * (y_pos blockPosition + y)

displayDoor :: Int -> Door -> Picture
displayDoor k Door{..} =
  (if doorSide `elem` [North, South]
   then translate (fromIntegral $ k * doorDistance) 0
   else translate 0 (fromIntegral $ k * doorDistance))
  $ displayBlock k
  $ (if doorSide `elem` [North, South]
    then Block 1 doorSize
    else Block doorSize 1) black
  $ Position 0 0


displayWall :: Int -> Board -> Picture
displayWall k Board{..} =
  Pictures $ fmap translateWall $
    [ (North, wallH), (South, wallH), (East, wallV), (West, wallV)
    , (doorSide door, displayDoor k door) ]
  where
    w     = fromIntegral (k * boardWidth)
    h     = fromIntegral (k * boardHeight)
    mk    = fromIntegral (-k)
    wallH = displayBlock k $ Block 1 (2 + boardWidth) (greyN 0.2) $ Position 0 0
    wallV = displayBlock k $ Block (2 + boardHeight) 1 (greyN 0.2) $ Position 0 0

    translateWall (side, r) = translate x y r
      where
        x     = if side == East  then w else mk
        y     = if side == North then h else mk

displayBoard :: Int -> Board -> Picture
displayBoard k b@Board{..} =
  translate (-w/2) (-h/2) $ Pictures $ displayWall k b : tiles : fmap (displayBlock k) content
  where
    w         = fromIntegral (k * boardWidth)
    h         = fromIntegral (k * boardHeight)

    wall      =
      foldl (\ ((x, y), d, ls) (x', y') ->
               let s = abs (x-x' + y-y') in
               if s < d then
               ((x', y'), d - s, line [ (x,y), (x', y') ] : ls)
               else undefined
               )
        ((-0.5, -0.5), fromIntegral (k * doorDistance door), [])
        [ (-0.5, w+0.5), (h+0.5, w+0.5), (h+0.5, -0.5), (-0.5, -0.5) ]

    tiles     =
      Color (greyN 0.2) $ Pictures $
      fmap line               (lines boardWidth boardHeight) ++
      fmap (line . fmap swap) (lines boardHeight boardWidth)

    lines n l = [ [ (x, 0), (x, fromIntegral $ k * l) ]
                | x <- fmap fromIntegral [0,k..(k*n)] ]
