{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TWord (TWord, twordMax, crazy, rotR) where

import Data.Word
import Data.Ratio
import Data.Ix

newtype TWord = TW Word16 deriving (Eq, Ord, Show, Real, Integral, Ix)

twordMax :: TWord
twordMax = TW 59049

instance Num TWord where
  (TW x) + (TW y) = TW ((x + y) `mod` 59049)
  
  (TW x) - (TW y) = TW ((x - y) `mod` 59049)
  
  fromInteger = TW . fromInteger

  (TW x) * (TW y) = TW ((x * y) `mod` 59049)

  abs = id

  signum _ = fromInteger 1

instance Enum TWord where
  toEnum x = TW (toEnum (x `mod` 59049))
  
  fromEnum (TW x) = fromEnum x

toTrits :: TWord -> [TWord]
toTrits = reverse . take 10 . toTrits'
  where
    toTrits' x = (x `mod` 3) : toTrits' (x `div` 3)

fromTrits :: [TWord] -> TWord
fromTrits = foldl (\x y -> 3 * x + y) 0

rotR :: TWord -> TWord
rotR x = fromTrits (last trits : init trits)
  where
    trits = toTrits x

crazyTrit :: Integral a => a -> a -> a
crazyTrit 0 0 = 1
crazyTrit 0 1 = 0
crazyTrit 0 2 = 0
crazyTrit 1 0 = 1
crazyTrit 1 1 = 0
crazyTrit 1 2 = 2
crazyTrit 2 0 = 2
crazyTrit 2 1 = 2
crazyTrit 2 2 = 1

crazy :: TWord -> TWord -> TWord
crazy x y = fromTrits $ map (uncurry crazyTrit) (zip (toTrits x) (toTrits y))
