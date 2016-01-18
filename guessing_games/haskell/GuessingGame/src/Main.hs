module Main where

import System.Random (randomRIO)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Guess the number!"
  putStrLn "Please input your guess:"

  secretNumber <- randomRIO (1, 101 :: Int)
  loop secretNumber

loop secretNumber = do
  guessInput <- getLine

  case (readMaybe guessInput :: Maybe Int) of
    Nothing -> do
      putStrLn("Wrong input!")
      loop secretNumber
    Just guess -> do
      putStrLn $ "You guessed: " ++ (show guess)
      case (compare guess secretNumber) of
        LT -> do putStrLn("Too small!"); loop secretNumber
        GT -> do putStrLn("Too big!"); loop secretNumber
        EQ -> putStrLn("You win!")
