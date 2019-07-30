module Main where

import Control.Monad.State
import Data.Maybe
import System.Environment (getArgs)

import Interpreter
import Parser
import Tape

emptyTape = Tape [0,0..] 0 [0,0..]

main :: IO ((), BFTape)
main = getSourceFile >>=
          readFile >>=
            \p -> runStateT (interpret (parse p)) emptyTape

mayHead :: [a] -> Maybe a
mayHead []      = Nothing
mayHead (a:as)  = Just a

fileNotExist = "No source file"

getSourceFile :: IO String
getSourceFile = mayHead <$> getArgs >>=
                  \f -> pure $ fromMaybe (error fileNotExist) f
