import Data.Char

-- Exercises
-- 1.
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safetise fun = \x -> case x of
                          [] -> Nothing
                          _  -> Just (fun x)

safeHead = safetise head
safeTail = safetise tail
safeLast = safetise last
safeInit = safetise init

-- 2.
-- 3.
-- 4.

-- Exercises
-- 1.
asInt_fold :: String -> Int
asInt_fold str = snd (asInt_fold2 str)

asInt_fold2 str = foldr step (' ', 0) (zip str [length str-1, length str -2..])
         where step x acc | (fst acc == '-') = error "Not number!" -- do not deal with several '-'
                          | isDigit (fst x)  = (' ', (snd acc) + fromIntegral (digitToInt (fst x)) * (10 ^ (snd x)))
                          | (fst x) == '-'   = ('-', -(snd acc))
                          | otherwise        = error "Not number!"

-- 2.
asInt_fold_maybe = safetise asInt_fold

-- 3.
concat :: [[a]] -> [a]
concat a = foldr (++) [] a

-- 4.
takeWhile_r :: (a -> Bool) -> [a] -> [a]
takeWhile_r fun []     = []
takeWhile_r fun (x:xs) = if fun x
                       then x : takeWhile_r fun xs
                       else []

takeWhile_f :: (a -> Bool) -> [a] -> [a]
takeWhile_f fun x = foldr step [] x
       where step elem acc = if fun elem
                             then elem : acc
                             else []

-- 5.
groupBy_f :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_f fun lst = foldr step [[]] lst
       where step x acc | null acc || null (head acc) = [[x]]
                        | fun x (head (head acc))     = [(head acc) ++ [x]] ++ tail acc
                        | otherwise                   = [[x]] ++ acc

-- 6.
any_f fun lst = foldr step False lst
        where step x acc = acc || fun x

-- cycle_f: don't think it's possible

-- words_f: similar to groupBy_f

unlines_f = foldr (\x y -> x ++ "\n" ++ y) ""


