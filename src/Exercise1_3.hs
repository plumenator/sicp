module Exercise1_3 where

main = putStrLn $ show $ sumOfSquaresOfLarger 12 12 12

sumOfSquaresOfLarger x y z
    | x >= z && y >= z    = x*x + y*y
    | y >= x && z >= x    = z*z + y*y
    | otherwise           = x*x + z*z