module Exercise1_8 where

main                        = print $ cube (curt 0.000005)

curt                        = curtIter 1 0

curtIter guess prevGuess x
    | goodEnough guess prevGuess
                            = guess
    | otherwise             = curtIter newGuess guess x where
    newGuess                = improve guess x

improve guess x             = (x / square guess + 2 * guess) / 3

cube x                      = x * square x

goodEnough guess prevGuess  = abs (guess / prevGuess - 1) < 0.001    -- Look ma! Divide by zero is legal!

square x                    = x * x