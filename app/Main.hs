module Main (main) where
import System.Environment
import Data.Char (isDigit)

main :: IO (Int)
main = do
    list <- getArgs :: IO [String]
    launch list

    
launch :: [String] -> IO (Int)
launch args =
        myWolfram rule start printlines window move value
        where
            rule = getRule args;
            start = getParsedArgs args "--start" 0;
            printlines = getParsedArgs args "--lines" (-1);
            window = getParsedArgs args "--window" 80;
            move = getParsedArgs args "--move" 0;
            value = [["*"]];

isNumber :: String -> Bool
isNumber ('-':xs) = all isDigit xs
isNumber (x) = all isDigit x

myWolfram :: Int -> Int -> Int -> Int -> Int -> [[String]] -> IO (Int)
-- myWolfram  rule start printlines window move _ | rule == -84 || start == -84 || printlines == -84 || window == -84 ||move == -84 = return(-84)
-- myWolfram _ _ 0 _ _ _ = return(0)
myWolfram  rule start printlines window move value = wDisplay window move start (wCalcul rule start printlines window move value)

-- mapLineDebug :: [[String]] -> [[String]]
-- mapLineDebug value =  [[show (10 - ((length value) `mod` 10 ))]] ++ value ++ [[show (( 10 - length value) `mod` 10)]]

mapLine :: [[String]] -> [[String]]
mapLine value =  [["*"]] ++ value ++ [["*"]]

wCalcul :: Int -> Int -> Int -> Int -> Int -> [[String]] -> [[[String]]]
wCalcul _ _ (-1) _ _ value = iterate mapLine value
wCalcul _ start printlines _ _ value = take (printlines) (drop start (iterate mapLine value))

putSize :: Int -> String -> IO ()
putSize _ [] = return()
putSize 0 _ = putStr "\n"
putSize size (x:xs) = do
        putStr [x]
        putSize (size - 1) xs

putOverflow :: Int -> Int -> Int ->String -> IO ()
putOverflow _ 0 size (_:xs) = putSize size xs
putOverflow start overflow size (_:xs) = putOverflow start (overflow - 1) size xs

printByLine :: Int -> Int -> Int -> Int -> [[[String]]] -> IO ()
printByLine _ _ _ _ [] = return ()
printByLine index window move start (x:xs) | move < 0 = do
        putOverflow start (index - move) window ((concat (replicate ((window `div` 2) + 1)  " ")) ++  (concat (concat x)) ++  (concat (replicate ((window `div` 2) + 1) " ")) ++ (concat (replicate (-move) " ")))
        printByLine (index + 1) window move start xs
printByLine index window move start (x:xs) | move >= 0 = do
        putOverflow start index window ((concat (replicate move " ")) ++ (concat (replicate ((window `div` 2) + 1)  " ")) ++  (concat (concat x)) ++  (concat (replicate ((window `div` 2) + 1) " ")))
        printByLine (index + 1) window move start xs


wDisplay :: Int -> Int -> Int -> [[[String]]] -> IO (Int)
wDisplay window move start value = do
        printByLine start window move start value
        return 0


isNotArgs :: String -> Bool     
isNotArgs a 
        | a /= "--rule" && a /= "--start" && a /= "--lines" && a /= "--window" && a /= "--move"     = True
        | otherwise                                                                                 = False

getParsedArgs :: [String] -> String -> Int -> Int  
getParsedArgs [] _ defaultValue = defaultValue
getParsedArgs (x:xs) name defaultValue
        |   (x == name && (null xs) || ( isNotArgs x)) && not (isNumber x)    = -84
        |   x == name && not (null xs) && (isNumber (head xs))                =  read (head xs) :: Int
        |   otherwise                                                         = getParsedArgs  xs name defaultValue                                                                  

getRule :: [String] -> Int
getRule [] = -84
getRule (x:xs)
        |   (x =="--rule" && (null xs) || ( isNotArgs x)) && not (isNumber x)                                                           = -84
        |   x =="--rule" && not (null xs) && (isNumber (head xs)) && (read (head xs) :: Int) > 0 && (read (head xs) :: Int) < 257       = read (head xs) :: Int
        |   otherwise                                                                                                                   = getRule xs