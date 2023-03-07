module Main (main) where
import System.Environment
import Data.Char (isDigit)

main :: IO ()
main = do
    list <- getArgs :: IO [String]
    print (launch list)
    
launch :: [String] -> Int
launch args =
        myWolfram rule start lines window
        where
            rule = getRule args;
            start = getStart args;
            lines = getLines args;
            window = getWindow args;

isNumber :: String -> Bool
isNumber = all isDigit

myWolfram :: Int -> Int -> Int -> Int -> Int
myWolfram  a b c d =  a + b + c + d;
myWolfram _ _ _ _ = 42
    
isNotArgs :: String -> Bool
isNotArgs a 
        | a /= "--rule" && a /= "--start" && a /= "--lines" && a /= "--window"  = True
        | otherwise                                                             = False

getRule :: [String] -> Int
getRule [] = 84
getRule (x:xs)
        |   (x =="--rule" && (null xs) || ( isNotArgs x)) && not (isNumber x)                                                                                  = 84
        |   x =="--rule" && not (null xs) && (isNumber (head xs)) && (read (head xs) :: Int) > 0 && (read (head xs) :: Int) < 257       = read (head xs) :: Int
        |   otherwise                                                                                                                   = getRule xs

getStart :: [String] -> Int
getStart [] = 0
getStart (x:xs)
        |   ((x =="--start" && (null xs) || ( isNotArgs x))) && not (isNumber x)                 = 84
        |   x =="--start" && not (null xs) && (isNumber (head xs))      = read (head xs) :: Int
        |   otherwise                                                   = getStart xs

getLines :: [String] -> Int
getLines [] = 20
getLines (x:xs)
        |   (x =="--lines" && (null xs) || ( isNotArgs x)) && not (isNumber x)                 = 84
        |   x =="--lines" && not (null xs) && (isNumber (head xs))      = read (head xs) :: Int
        |   otherwise                                                   = getLines xs

getWindow :: [String] -> Int
getWindow [] = 80
getWindow(x:xs)
        |(x =="--window" && (null xs) || ( isNotArgs x)) && not (isNumber x)                  = 84
        |x =="--window" && not (null xs) && (isNumber (head xs))        = read (head xs) :: Int
        | otherwise                                                     = getWindow xs