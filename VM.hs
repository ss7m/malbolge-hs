import Data.Array
import Prelude hiding (Word)

import Trit
import Word

type Register = Word
type Memory = Array Word Word

initMemory :: Memory
initMemory = listArray (wordMin, wordMax) (repeat wordMin)

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
