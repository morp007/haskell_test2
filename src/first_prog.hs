module TT where

import           Data.Dynamic
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
