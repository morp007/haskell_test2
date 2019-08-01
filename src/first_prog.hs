module TT where

import Data.Dynamic
-- import Data.Typeable


-- getType x = show (typeOf x)
getType x = print (dynTypeRep (toDyn x))

toPart name = "dear " ++ name
bodyPart title = "book titile: " ++ title
fromPart author = "author: " ++ author

createMsg name title author = toPart name ++ "\n"
                              ++ bodyPart title ++ "\n"
                              ++ fromPart author ++ "\n"


messyMain :: IO()
messyMain = do
    print "enter email"
    email <- getLine
    print "enter title"
    title <- getLine
    print "enter author"
    author <- getLine
    print (createMsg email title author)
