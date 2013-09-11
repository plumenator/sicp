module Exercise1_7 where

main                        = print $ square (sqrt' 0.000005)

sqrt' x                     = sqrtIter 1 0 where
    sqrtIter guess prevGuess
        | goodEnough        = guess
        | otherwise         = sqrtIter newGuess guess where
        newGuess            = average guess (x / guess)
        goodEnough          = abs (guess / prevGuess - 1) < 0.001    -- Look ma! Divide by zero is legal!

average x y                 = (x + y) / 2

square x                    = x * x