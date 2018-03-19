module Trit (Trit(..), tritToInt, tritFromInt, addTrit3, subTrit3) where

import Prelude hiding (Word)

data Trit = O | W | T deriving (Eq)

instance Show Trit where
  show O = "0"
  show W = "1"
  show T = "2"

instance Ord Trit where
  x <= y = tritToInt x <= tritToInt y

-- converts trit to int
tritToInt :: Integral a => Trit -> a
tritToInt O = 0
tritToInt W = 1
tritToInt T = 2

-- converts int to trit
tritFromInt :: Integral a => a -> Trit
tritFromInt 0 = O
tritFromInt 1 = W
tritFromInt 2 = T

-- adds three trits together with overflow
addTrit3 :: Trit -> Trit -> Trit -> (Trit, Trit)
addTrit3 x y z = let xyz = tritToInt x + tritToInt y + tritToInt z in 
                 (tritFromInt (xyz `div` 3), tritFromInt (xyz `mod` 3))

-- subtracts to trits with borrow
subTrit3 :: Trit -> Trit -> Trit -> (Trit, Trit)
subTrit3 c x y 
  | res >= 0 = (O, tritFromInt res)
  | otherwise = (W, tritFromInt (res + 3))
  where
    c' = tritToInt c 
    x' = tritToInt x
    y' = tritToInt y
    res = x' - c' - y'
