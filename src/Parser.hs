module Parser where

data Token = NEXT_CELL
            | PREVIOUS_CELL
            | INCREMENT
            | DECREMENT
            | PUT
            | GET
            | SLOOP [Token] [Token]
            deriving (Show)

instructionsAfterLoop :: String -> String
instructionsAfterLoop = i' 0
                      where
                        i' 0 (']':is) = is
                        i' d ('[':is) = flip i' is $ succ d
                        i' d (']':is) = flip i' is $ pred d
                        i' d (_:is)   = i' d is

parse :: String -> [Token]
parse = flip parse' []
  where parse' (i:is) acc
          | i == '>' = parse' is $ NEXT_CELL:acc
          | i == '<' = parse' is $ PREVIOUS_CELL:acc
          | i == '+' = parse' is $ INCREMENT:acc
          | i == '-' = parse' is $ DECREMENT:acc
          | i == ',' = parse' is $ GET:acc
          | i == '.' = parse' is $ PUT:acc
          | i == '[' = reverse $ SLOOP (loopBody is) (loopEnd is) : acc
          | i == ']' = reverse acc
          | otherwise= parse' is acc
        parse' [] acc = reverse acc
        loopBody = flip parse' []
        loopEnd  = flip parse' [] . instructionsAfterLoop
