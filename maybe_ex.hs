listExclude :: Int -> [a] -> [a]
listExclude n xs = left ++ tail right
                    where (left,right) = splitAt n xs

getInput :: Read a => IO [a]
getInput = do
    line <- getLine 
    return $ map read $ words line

solve :: Int -> [Int] -> Int -> Maybe Int
solve k bill b  
    |b > actualPrice = Just (b - actualPrice)
    |otherwise = Nothing
    where actualPrice = sum listExclude k bill 'div' 2

main :: IO ()
main = do
    [_, k] <- getInput
    bill <- getInput
    [b] <- getInput
    putStrLn $ maybe "Bon Appetite" $ show $ solve k bill b 
