import Data.Maybe(mapMaybe)
import Text.Read (readMaybe)

data Person = Person {
    name :: String,
    surname :: String,
    father :: Maybe Person,
    mother :: Maybe Person
} deriving Show

jane = Person "Jane" "Smith" Nothing Nothing
russ = Person "Russ" "Cox" Nothing Nothing
miranda = Person "Miranda" "Lee" Nothing Nothing
alice = Person "Alice" "Cox" (Just russ) (Just jane)
john = Person "John" "Lee" Nothing (Just miranda)
david = Person "David" "Lee" (Just john) (Just alice)

-- 1
mothersFather' :: Person -> Maybe Person
mothersFather' person = case mother person of
    Just mom -> father mom
    Nothing  -> Nothing
    
mothersFather :: Person -> Maybe Person
mothersFather person = do
    mom <- mother person
    father mom
    
-- 2
hasGrandParents :: Person -> Maybe Person
hasGrandParents person = do
    grandpa1 <- mothersFather person
    grandma1 <- mothersFather person
    grandpa2 <- mothersFather person
    grandma2 <- mothersFather person
    return person
    
-- 3
main :: IO ()
main = 
    getLine >>= \input ->
    let nums = mapMaybe readMaybe (words input)
    in case nums of
        [x, y] -> print (x + y)
        _      -> putStrLn "err"
