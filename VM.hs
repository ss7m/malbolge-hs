module VM (Register, Memory, State, encrypt, chr', ord') where

import Data.Char
import Data.Array.IO
import Data.List (elemIndex)

import TWord

type Register = TWord
type Memory = IO (IOArray TWord TWord)
type State = (Register, Register, Register, Memory)

ord' :: Char -> TWord
ord' = toEnum . ord

chr' :: TWord -> Char
chr' = chr . fromEnum

encryptIn :: String
encryptIn = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" 

encryptOut :: String
encryptOut = "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1CB6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@"

indexUnsafe :: Char -> String -> Int
indexUnsafe c str = case elemIndex c str of Just x  -> x
                                            Nothing -> 0
encrypt :: TWord -> TWord
encrypt = ord' . (encryptOut !!) . (`indexUnsafe` encryptIn) . chr'
