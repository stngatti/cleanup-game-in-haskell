{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use and" #-}
import System.Random ( mkStdGen, Random(randomRs), newStdGen ) 
import Data.List ()
import System.Environment ()
import Data.List.Split ( chunksOf )
import Control.Monad ( when )

cleanup :: (Int, Int) -> [Bool] -> Int -> [Bool]
cleanup (cols, rows) matrix centre =
  let (x, y) = centre `divMod` cols
      indices = [i * cols + j | i <- [x-1..x+1], i >= 0, i < cols-1, j <- [y-1..y+1], j >= 0, j < rows-1, abs (i-x) + abs (j-y) == 1]
      updateValue [] _ _ = []
      updateValue (x:xs) indexes acc
        | acc `elem` indexes = not x : updateValue xs indexes (acc+1)
        | otherwise = x : updateValue xs indexes (acc+1)
  in updateValue matrix indices 0

getTable :: (Int, Int) -> [Int] -> [Bool] -> [Bool]
getTable (w, h) xs matrix = foldl (\m x -> cleanup (w, h) m x) matrix xs 

getRandomDistinctList :: Int -> Int -> IO[Int]
getRandomDistinctList n maxNum = do
  gen <- newStdGen
  let nums = take n $ randomRs (1, maxNum) gen
  return $ take n $ getDistinct nums []

getDistinct :: Eq a => [a] -> [a] -> [a]
getDistinct [] acc = acc
getDistinct (x:xs) acc
  | x `elem` acc = getDistinct xs acc
  | otherwise    = getDistinct xs (x:acc)

printMatrix :: (Int, Int) -> [Bool] -> IO ()
printMatrix (w, h) matrix = do
  let rows = chunksOf w matrix
  mapM_ (putStrLn . concatMap (\cell -> if cell then "X " else "O ")) rows

startGame :: (Int, Int) -> Int -> IO ()
startGame (w, h) m = do
  let clearMatrix = replicate (w * h) True
  randomIndices <- getRandomDistinctList m (w * h)
  let startingMatrix = getTable (w, h) randomIndices clearMatrix
  putStrLn "Initial Matrix:"
  printMatrix (w, h) startingMatrix
  playLoop (w, h) startingMatrix 0

playLoop :: (Int, Int) -> [Bool] -> Int -> IO ()
playLoop (w, h) matrix numMoves = do
  putStrLn "Enter row and column (separated by a space) to make a move, or 'q' to quit: "
  input <- getLine
  when (input /= "q") $ do
    let coords = map read $ words input
        cellIndex = (head coords * w) + coords !! 1
    if cellIndex >= (w * h) || cellIndex < 0
      then do
        putStrLn "Invalid move. Please try again."
        playLoop (w, h) matrix numMoves
      else do
        let newMatrix = cleanup (w, h) matrix cellIndex
        putStrLn "Updated matrix:"
        printMatrix (w, h) newMatrix
        if all id newMatrix
          then putStrLn $ "Congratulations! You won in " ++ show (numMoves + 1) ++ " moves."
          else playLoop (w, h) newMatrix (numMoves + 1)

main :: IO ()
main = do
  putStrLn "Enter the number of row and columns and the difficulty level:"
  input <- getLine
  let [w, h, m] = map read $ words input
  startGame(w, h) m
  
