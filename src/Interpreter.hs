module Interpreter where

import Data.Char (chr, ord)
import Control.Monad.State
import Data.Word (Word8)

import Parser
import Tape

type Program = [Token]
type BFTape = Tape Word8

toString :: Char -> String
toString c = [c]

interpret :: Program -> StateT BFTape IO ()
interpret (NEXT_CELL:is)      = modify nextCell >> interpret is
interpret (PREVIOUS_CELL:is)  = modify previousCell >> interpret is
interpret (INCREMENT:is)      = modify incrementCell >> interpret is
interpret (DECREMENT:is)      = modify decrementCell >> interpret is
interpret (PUT:is)            = get >>=
                                  lift . putStr . toString . chr . fromEnum . getValue
                                  >> interpret is
interpret (GET:is)            = lift getChar >>=
                                  modify . putCell . toEnum . ord
                                  >> interpret is
interpret (SLOOP b e : is)    = gets getValue >>=
                                  \c -> if c == 0
                                    then interpret e
                                    else interpret b
                                  >> interpret (SLOOP b e: is) 
interpret []                  = pure ()
