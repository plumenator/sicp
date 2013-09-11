module NewtonSqrt where

main                        = print $ square (sqrt' 1000)

sqrt'                        = sqrtIter 1

sqrtIter guess x
    | goodEnough guess x    = guess
    | otherwise             = sqrtIter (improve guess x) x

improve guess x             = average guess (x / guess)

average x y                 = (x + y) / 2

goodEnough guess x          = abs (square guess - x) < 0.001

square x                    = x * x