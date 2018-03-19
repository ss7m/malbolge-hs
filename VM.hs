module VM (Register, Memory, State, crazyW, rot, ord', chr', encrypt) where

import Data.Char
import Data.Array.IO
import Data.List (elemIndex)
import Prelude hiding (Word)

import Trit
import Word

type Register = Word
type Memory = IO (IOArray Word Word)
type State = (Register, Register, Register, Memory)

rot :: Word -> Word
rot [] = error "What are you even doing"
rot (x:xs) = xs ++ [x]

crazyT :: Trit -> Trit -> Trit
crazyT O O = W
crazyT O W = O
crazyT O T = O
crazyT W O = W
crazyT W W = O
crazyT W T = T
crazyT T O = T
crazyT T W = T
crazyT T T = W

crazyW :: Word -> Word -> Word
crazyW x y = map (uncurry crazyT) $ zip x y

ord' :: Char -> Word
ord' = toEnum . ord

chr' :: Word -> Char
chr' = chr . fromEnum

encryptIn :: String
encryptIn = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" 

encryptOut :: String
encryptOut = "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1CB6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@"

indexUnsafe :: Char -> String -> Int
indexUnsafe c str = case elemIndex c str of Just x  -> x
                                            Nothing -> 0

encrypt :: Word -> Word
encrypt = ord' . (encryptOut !!) . (`indexUnsafe` encryptIn) . chr'
