{-# LANGUAGE FlexibleInstances #-}

module Word (Word, wordMin, wordMax) where

import Prelude hiding (Word)
import Data.Ratio
import Data.Ix

import Trit

type Word = [Trit] -- I'll just have to assert length of 10

-- adds two words together
addWord :: Word -> Word -> Word
addWord = addWord' O
  where
    addWord' :: Trit -> Word -> Word -> Word
    addWord' _ [] [] = []
    addWord' _ _  [] = error "Words must be of length 10"
    addWord' _ [] _  = error "Words must be of length 10"
    addWord' carry (x:xs) (y:ys) =
      let (newCarry, res) = addTrit3 carry x y in
      res : addWord' newCarry xs ys

-- subtract two words
subWord :: Word -> Word -> Word
subWord = subWord' O
  where
    subWord' :: Trit -> Word -> Word -> Word
    subWord' _ [] [] = []
    subWord' _ _  [] = error "Words must be of length 10"
    subWord' _ [] _  = error "Words must be of length 10"
    subWord' carry (x:xs) (y:ys) =
      let (newCarry, res) = subTrit3 carry x y in
      res : subWord' newCarry xs ys

instance Num Word where
  (+) = addWord
  (-) = subWord
  
  fromInteger =
    let fromInteger' x = tritFromInt (x `mod` 3) : fromInteger' (x `div` 3) in
    take 10 . fromInteger'

  x * y = if x == fromInteger 0 then y else y + (x - fromInteger 1) * y

  abs = id

  signum _ = fromInteger 1

instance Enum Word where
  toEnum =
    let toEnum' x = tritFromInt (x `mod` 3) : toEnum' (x `div` 3) in
    take 10 . toEnum'

  fromEnum [] = 0
  fromEnum (x:xs) = tritToInt x + 3 * fromEnum xs

  succ x = x + [W,O,O,O,O,O,O,O,O,O]

instance Real Word where
  toRational [] = 0
  toRational (x:xs) = (tritToInt x % 1) + 3 * toRational xs

instance Integral Word where
  toInteger [] = 0
  toInteger (x:xs) = tritToInt x + 3 * toInteger xs

  quotRem x y = if y == fromInteger 0 then error "Divide by 0" else quotRem' x y
    where
      quotRem' :: Word -> Word -> (Word, Word)
      quotRem' x y
        | reverse x < reverse y = (fromInteger 0, x)
        | otherwise = let (quot, rem) = quotRem' (x-y) y in (quot+1, rem)

instance Ix Word where
  range (x,y) = take (1+fromEnum (y-x)) (iterate succ x)

  inRange (x,y) z = z' >= x' && z' <= y' 
    where
      x' = reverse x
      y' = reverse y
      z' = reverse z

  index (x,y) z
    | inRange (x,y) z = fromEnum (z-x)
    | otherwise = error ("Error: " ++ show z ++ "Out of range")

wordMax :: Word
wordMax = [T,T,T,T,T,T,T,T,T,T]

wordMin :: Word
wordMin = [O,O,O,O,O,O,O,O,O,O]
