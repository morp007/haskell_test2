module TT where

import           Control.Arrow
import           Data.Char
import           Data.Dynamic
import           Data.Function
import           Data.List
import           Data.Maybe
-- import Data.Typeable


-- getType x = show (typeOf x)
getType x = print (dynTypeRep (toDyn x))

toPart name = "dear " ++ name
bodyPart title = "book titile: " ++ title
fromPart author = "author: " ++ author

createMsg name title author =
    toPart name ++ "\n" ++ bodyPart title ++ "\n" ++ fromPart author ++ "\n"


messyMain :: IO ()
messyMain = do
    print "enter email"
    email <- getLine
    print "enter title"
    title <- getLine
    print "enter author"
    author <- getLine
    print (createMsg email title author)


calcChange owed given = if x > 0 then x else 0 where x = given - owed

square x = x ^ 2

oddOrEven :: Int -> String
oddOrEven x = if rem x 2 == 0 then "even" else "odd"

sumSquareOrSquareSum a b =
    (\sumSquare squareSum ->
            if sumSquare > squareSum then sumSquare else squareSum
        )
        ((a ^ 2) + (b ^ 2))
        ((a + b) ^ 2)


doubleDouble x = dubs * 2 where dubs = x * 2

ddx x = d (d x) where d = \x -> x * 2

overwrite x = let x = 2 in let x = 3 in let x = 4 in x

-- lesson 3.4
l34_x = 4
l34_add1 y = y + l34_x
l34_add2 y = (\l34_x -> y + l34_x) 3
l34_add3 y = (\y -> (\l34_x -> l34_x + y) 1) 2

-- lesson 4
-- listing 4.5
l45_compareLastName x y | compareRes == EQ = fst x `compare` fst y
                        | otherwise = compareRes
                        where compareRes = snd x `compare` snd y


l45_abstractCompare' [] [] = EQ
l45_abstractCompare' _ [] = GT
l45_abstractCompare' [] _ = LT
l45_abstractCompare' (x:xs) (y:ys) | compareRes == EQ = xs `l45_abstractCompare'` ys
                           | otherwise = compareRes
                           where
                               compareRes = x `compare` y
l45_abstractCompare :: Ord a => [a] -> [a] -> Ordering
l45_abstractCompare x y = reverse x `l45_abstractCompare'` reverse y


fizzBuzz n =
    let
        base divisor s =
            if (n `mod` divisor) == 0
                then Just s
                else Nothing
        maybeFizz = base 3 "Fizz"
        maybeBuzz = base 5 "Buzz"
    in fromMaybe (show n) (maybeFizz <> maybeBuzz)


-- см. https://habr.com/ru/post/470407/#comment_20729322
--
-- Необходимо реализовать функцию getRanges, которая возвращает следующие результаты
-- getRanges([0, 1, 2, 3, 4, 7, 8, 10]) // "0-4,7-8,10"
-- getRanges([4,7,10]) // "4,7,10"
-- getRanges([2, 3, 8, 9]) // "2-3,8-9"
--
getRanges :: [Int] -> [(Int, Int)]
getRanges = foldr go ([])  -- делаем reduce
  where
    go x t = case t of -- берем очередной элемент и аккумулятор
        ((l, r) : as) | l - x == 1 -> ((x, r) : as) -- если там что-то есть и можем всунуть, то расширяем ренж текущим элементом
        _                          -> (x, x) : t     -- иначе создаем новый интервал

getRangesAndPrint_v1 = print . getRanges

-- см. https://habr.com/ru/post/470660/#comment_20729904
getRanges_v2 = map ((head &&& last) . map snd) . groupBy ((==) `on` fst) . zipWith
    (\i v -> (v - i, v))
    [0 ..]

-- -- типа другой вывод
-- main = do
--   let ranges = getRanges [0, 1, 2, 3, 4, 7, 8, 10]
--   let format = fmap $ \x -> case x of
--                 (a, b) | a == b -> $"{a}"
--                 (a, b) -> $"{a}-{b}"
--   print $ format ranges

ifEven myFunction x = if even x then myFunction x else x


qc51_genIfXEven x = \myFunction -> ifEven myFunction x


qc54_subtract2 = flip (-) 2


q62_subseq start end list = case compare start end of
    LT -> (list !! start) : q62_subseq (start + 1) end list
    _ -> []


-- возвращает True, если элемент в 1 половине списка С УЧЕТОМ граничного элемента
q63_inFirstHalf :: Eq a => a -> [a] -> Bool
q63_inFirstHalf value list =
    case compare (valueIndex * 2) listLength of
        LT -> True
        _  -> False
    where
        listLength = length list
        valueIndex = fromMaybe listLength $ Data.List.elemIndex value list
-- возвращает True, если элемент в 1 половине списка НЕ УЧИТЫВАЯ граничный элемент
-- (пример из книги)
q63_inFirstHalf_fromBook val myList = val `elem` firstHalf
    where
        midpoint = length myList `div` 2
        firstHalf = take midpoint myList


qc73_myTail (_:xs) = xs


q71_myTail :: [a] -> [a]
q71_myTail [] = []              -- обязательно идет 1
q71_myTail x = qc73_myTail x


qc81_myLength :: [a] -> Int
qc81_myLength [] = 0
qc81_myLength (_:xs) = 1 + qc81_myLength xs


q81_reverse :: [a] -> [a]
q81_reverse [] = []
q81_reverse (x:xs) = q81_reverse xs ++ [x]


q82_fib :: Int -> Int
q82_fib 0 = 0
q82_fib 1 = 1
q82_fib n = q82_fib (n-1) + q82_fib (n-2)

q82_fastFib_fromBook' :: Int -> Int -> Int -> Int
q82_fastFib_fromBook' _ _ 0 = 0
q82_fastFib_fromBook' _ _ 1 = 1
q82_fastFib_fromBook' _ _ 2 = 1
q82_fastFib_fromBook' x y 3 = x + y
q82_fastFib_fromBook' x y c = q82_fastFib_fromBook' (x + y) x (c - 1)

q82_fastFib_fromBook :: Int -> Int
q82_fastFib_fromBook = q82_fastFib_fromBook' 1 1


q91_elem :: Eq a => a -> [a] -> Bool
q91_elem value xs = (/=) 0 $ length $ filter (== value) xs


q92_isPalindrome :: String -> Bool
q92_isPalindrome xs = words == reverse words
        where words = filter (/= ' ') $ map toLower xs


q93_harmonic 0 = 0
q93_harmonic x = (1 / x) + q93_harmonic (x - 1)

q93_harmonic_fromBook  n = sum (take n seriesValues)
    where
        seriesPairs = zip (repeat 1.0) [1.0, 2.0 .. ]
        seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs


type L12_PatientName = (String, String)
type L12_Age = Int
type L12_Height = Int

qc121_patientInfo :: L12_PatientName -> L12_Age -> L12_Height -> String
qc121_patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
    where
        name = fname ++ ", " ++ lname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
