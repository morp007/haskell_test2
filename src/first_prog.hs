module TT where

import           Data.Dynamic
import           Data.List
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
l45_abstractCompare' rx ry | compareRes == EQ = tail rx `l45_abstractCompare'` tail ry
                           | otherwise = compareRes
                           where
                               compareRes = head rx `compare` head ry
l45_abstractCompare :: Ord a => [a] -> [a] -> Ordering
l45_abstractCompare x y = reverse x `l45_abstractCompare'` reverse y
