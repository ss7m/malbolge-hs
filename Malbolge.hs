import Data.Char
import Data.Array.MArray
import Data.Foldable
import System.IO
import System.Environment
import System.Exit

import TWord
import VM

-- Takes the code and initializes the memory
initializeMemory :: String -> Memory
initializeMemory str = do
  mem <- newArray (0, twordMax) 0 :: Memory
  let str' = filter (not . isSpace) str
  let prog = zip (enumFromTo 0 twordMax) (map ord' str')

  forM_ prog $ \(i,e) -> 
    if ((e + i) `mod` 94) `elem` [4,5,23,39,40,62,68,81] then
      writeArray mem i e
    else do
      print "Invalid character in source file"
      exitFailure

  forM_ (enumFromTo (toEnum (length str')) twordMax) $ \i -> do
    w1 <- readArray mem (i - 1)
    w2 <- readArray mem (i - 2)
    writeArray mem i (crazy w2 w1)

  return mem

-- creates the initial state
initializeState :: String -> State
initializeState str = (0, 0, 0, initializeMemory str)

-- Runs the malbolge program
run :: State -> IO ()
run (a,c,d,iomem) = do
  mem <- iomem
  _c_ <- readArray mem c

  let instr = (c + _c_) `mod` 94

  if instr == 4 then do
    _d_ <- readArray mem d
    next (a, _d_, d, return mem)
  else if instr == 5 then do
    putChar (chr' (a `mod` 128))
    next (a, c, d, return mem)
  else if instr == 23 then do
    newA <- getChar
    next (ord' newA, c, d, return mem)
  else if instr == 39 then do
    _d_ <- readArray mem d
    let rotD = rotR _d_
    writeArray mem d rotD
    next (rotD, c, d, return mem)
  else if instr == 40 then do
    _d_ <- readArray mem d
    next (a, c, _d_, return mem)
  else if instr == 62 then do
    _d_ <- readArray mem d
    let crz = crazy _d_ a
    writeArray mem d crz
    next (crz, c, d, return mem)
  else if instr == 81 then do
    return ()
  else do
    next (a, c, d, return mem)

-- handles incrementing and encryption
next :: State -> IO ()
next (a,c,d,iomem) = do
  mem <- iomem
  _c_ <- readArray mem c
  writeArray mem c $ encrypt (_c_ `mod` 94)
  run (a,c+1,d+1, return mem)

main :: IO ()
main = do
  args <- getArgs
  code <- readFile (head args)
  run (initializeState code)
