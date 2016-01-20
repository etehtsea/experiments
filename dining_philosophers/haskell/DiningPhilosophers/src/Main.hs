module Main where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_)
import System.Random (randomRIO)

type Fork = MVar Int
type Seat = Int
data Philosopher = Philosopher { name :: String
                               , seat :: Seat
                               , forkNums :: (Int, Int) }

sleep :: Int -> IO ()
sleep n = do
  delay <- randomRIO (1, n * 10)
  threadDelay (delay * 100000)

forkPair :: [Fork] -> (Int, Int) -> (Fork, Fork)
forkPair forks (x, y) = do
  let first = forks !! x
  let second = forks !! y
  (first, second)

mkFork :: Int -> IO Fork
mkFork i = newMVar i

takeFork :: Fork -> IO Int
takeFork fork = takeMVar fork

putFork :: Fork -> Int -> IO ()
putFork fork i = putMVar fork i

mkPhil :: Seat -> Philosopher
mkPhil seat' = Philosopher (genName seat') seat' (genForkNums seat')
  where genName seatNum = "PH" ++ (show seatNum)
        genForkNums seatNum = if left < right then (left, right) else (right, left)
          where left = seatNum `mod` 5
                right = (seatNum + 1) `mod` 5

revive :: [Fork] -> (Philosopher, IO Int) -> IO ()
revive forks (phil, timesToEat) = do
  let (left, right) = forkPair forks (forkNums phil)

  -- pick forks
  leftNum <- takeFork left
  rightNum <- takeFork right
  putStrLn $ name phil ++ " took " ++ "(" ++ (show leftNum) ++ "," ++ (show rightNum) ++ ")"

  -- eat
  sleep 1

  timesToEatVal <- timesToEat
  let eatLeft = pred timesToEatVal
  putStrLn $ "-------- " ++ (name phil) ++ " " ++ (show eatLeft) ++ " times to eat left"

  -- put forks
  putFork left leftNum
  putFork right rightNum
  putStrLn $ name phil ++ " put " ++ "(" ++ (show leftNum) ++ "," ++ (show rightNum) ++ ")"

  -- think
  putStrLn $ (name phil) ++ " is thinking!"
  sleep 1

  if (eatLeft > 0)
  then
    revive forks (phil, return eatLeft)
  else do
    putStrLn $ ">==========> " ++ name phil ++ " finished!"
    return ()

main :: IO ()
main = do
  let philosophersAmount = 25
  let philosophers = [mkPhil s | s <- [0..pred philosophersAmount]]
  let philosophersWithEatAmount = map addTimesToEat philosophers
  forks <- mapM mkFork [0..4]

  forM_ philosophersWithEatAmount (forkIO.(revive forks))

  loop
  where addTimesToEat ph = (ph, genTimesToEat)
          where genTimesToEat = randomRIO (5, 10) :: IO Int
        loop = do
          input <- getLine
          if input == "exit" then return () else loop
