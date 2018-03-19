import VM
import Word
import Data.Char
import Data.Array.MArray
import Data.Foldable
import System.IO
import System.Environment

initializeMemory :: String -> Memory
initializeMemory str = do
  mem <- newArray (wordMin, wordMax) wordMin :: Memory
  let prog = zip (enumFromTo wordMin wordMax) (map ord' (filter (not . isSpace) str))

  mapM_ (\(i,e) -> writeArray mem i e) prog

  forM_ (enumFromTo (toEnum (length str)) wordMax) $ \i -> do
    w1 <- readArray mem (i - 1)
    w2 <- readArray mem (i - 2)
    writeArray mem i (crazyW w1 w2)

  return mem

initializeState :: String -> State
initializeState str = (wordMin, wordMin, wordMin, initializeMemory str)

run :: State -> IO ()
run (a,c,d,iomem) = do
  print (a,c,d)
  mem <- iomem
  _c_ <- readArray mem c
  -- check out of range
  let instr = (c + _c_) `mod` 94
  if instr == 4 then do
    print 4
    _d_ <- readArray mem d
    next (a, _d_, d, iomem)
  else if instr == 5 then do
    print 5
    print (chr' (a `mod` 256))
    next (a, c, d, iomem)
  else if instr == 23 then do
    print 23
    newA <- getChar
    next (ord' newA, c, d, iomem)
  else if instr == 39 then do
    print 39
    _d_ <- readArray mem d
    let rotD = rot _d_
    writeArray mem d rotD
    next (rotD, c, d, iomem)
  else if instr == 40 then do
    print 40
    _d_ <- readArray mem d
    next (a, c, _d_, iomem)
  else if instr == 62 then do
    print 62
    _d_ <- readArray mem d
    let crz = crazyW a _d_
    writeArray mem d crz
    next (crz, c, d, iomem)
  else if instr == 81 then do
    print 81
    return ()
  else
    next (a, c, d, iomem)

next :: State -> IO ()
next (a,c,d,iomem) = do
  mem <- iomem
  _c_ <- readArray mem c
  writeArray mem c (encrypt _c_)
  run (a,c+1,d+1, iomem)

main :: IO ()
main = do
  args <- getArgs
  code <- readFile (head args)
  run (initializeState code)
