module Exercise1_7 where

main                        = print $ square (sqrt' 0.000005)

sqrt'                       = sqrtIter 1

sqrtIter guess x
    | goodEnough newGuess guess
                            = guess
    | otherwise             = sqrtIter newGuess x where
    newGuess                = improve guess x

improve guess x             = average guess (x / guess)

average x y                 = (x + y) / 2

goodEnough newGuess guess   = abs (newGuess / guess - 1) < 0.001

square x                    = x * x