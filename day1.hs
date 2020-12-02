import System.IO

main :: IO ()
main = do
    input <- readFile "day1_input.txt"
    let singlewords = words input
        list = f singlewords
        result1 = multiply (findTwoElementsThatSum 2020 list)
        result2 = multiply (findThreeElementsThatSum 2020 list)
    print (result1, result2)


f :: [String] -> [Integer]
f = map read

findTwoElementsThatSum :: Integer -> [Integer] -> [Integer]
findTwoElementsThatSum y [] = []
findTwoElementsThatSum y (x:[]) = []
findTwoElementsThatSum y (x:xs) = if [] == filtered
                                 then findTwoElementsThatSum y xs
                                 else x:filtered
                        where filtered = filter (\n -> x + n == y) xs


findThreeElementsThatSum :: Integer -> [Integer] -> [Integer]
findThreeElementsThatSum y [] = []
findThreeElementsThatSum y (x:[]) = []
findThreeElementsThatSum y (x:xs) = if [] == filtered
                                    then findThreeElementsThatSum y xs
                                    else x:filtered
                        where filtered = findTwoElementsThatSum (y-x) xs

multiply :: [Integer] -> Integer
multiply xs = foldr (*) 1 xs

