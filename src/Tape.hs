module Tape where

data Tape a = Tape [a] a [a]
              deriving (Show)

modifyTape :: (a -> a) -> Tape a -> Tape a
modifyTape f (Tape l c r) = Tape l (f c) r

nextCell :: Tape a -> Tape a
nextCell (Tape l c (r:rs)) = Tape (c:l) r rs

previousCell :: Tape a -> Tape a
previousCell (Tape (l:ls) c r) = Tape ls l (c:r)

incrementCell :: Enum a => Tape a -> Tape a
incrementCell = modifyTape succ

decrementCell :: Enum a => Tape a -> Tape a
decrementCell = modifyTape pred

putCell :: a -> Tape a -> Tape a
putCell c (Tape l _ r) = Tape l c r

getValue :: Tape a -> a
getValue (Tape _ c _) = c
