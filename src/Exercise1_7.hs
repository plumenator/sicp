module Exercise1_7 where

main                        = print $ square (sqrt' 0.000005)

sqrt'                       = sqrtIter 1 0

sqrtIter guess prevGuess x
    | goodEnough guess prevGuess
                            = guess
    | otherwise             = sqrtIter newGuess guess x where
    newGuess                = improve guess x

improve guess x             = average guess (x / guess)

average x y                 = (x + y) / 2

goodEnough guess prevGuess  = abs (guess / prevGuess - 1) < 0.001

square x                    = x * x