{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib.Crossword where

import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as Map
import Control.Lens
import Control.Arrow
import Data.Traversable
import Data.Foldable
import Data.Map.Lens

data Crossword = Crossword { _getMap :: [((Int, Int), CWord)]}

data CWord = CWord { _dir   :: Dir
                   , _chars :: String
                   }

data Dir = Vert | Hor

makeLenses ''CWord
makeLenses ''Crossword

instance Show Crossword where
  show cr = let (x, y) = dimens cr
                cm     = toCharMap cr
            in intercalate "\n"
    [[c | i <- [0..x], let c = fromMaybe ' ' (cm ^.at (i, j))] | j <- [0..y]]

toCharMap :: Crossword -> Map.Map (Int, Int) Char
toCharMap = fold . fmap (uncurry getCharPositions) . _getMap

getCharPositions :: (Int, Int) -> CWord -> Map.Map (Int, Int) Char
getCharPositions i w = Map.fromList $ getCharPositions' i w where
  getCharPositions' (x, y) (CWord Vert s) = map (\(i, c) -> ((x, y+i), c)) (zip [0..] s)
  getCharPositions' (x, y) (CWord Hor  s) = map (\(i, c) -> ((x+i, y), c)) (zip [0..] s)

dimens :: Crossword -> (Int, Int)
dimens c@(Crossword m) | null m    = (0, 0)
                       | otherwise = ((maximum *** maximum) . unzip . Map.keys . toCharMap) c

example :: Crossword
example = Crossword [((0, 0), CWord Vert "TEST"), ((0, 0), CWord Hor "TAK"), ((0, 2), CWord Hor "SMAK")]
