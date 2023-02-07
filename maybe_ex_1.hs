list_break :: Int -> [a] -> [a]
list_break x xs = left ++ tail right
                  where (left, right) = splitAt x xs 


