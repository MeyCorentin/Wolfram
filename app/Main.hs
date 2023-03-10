module Main (main) where
import System.Environment
import Data.Char (isDigit)

main :: IO (Int)
main = do
    list <- getArgs :: IO [String]
    launch list

intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin value = reverse (ruleBinary value)

ruleBinary :: Int -> [Int]
ruleBinary 0 = []
ruleBinary n    | n `mod` 2 == 1 = ruleBinary (n `div` 2) ++ [1]
                | n `mod` 2 == 0 = ruleBinary (n `div` 2) ++ [0]

addZero :: [Int] -> [Int]
addZero (list) = if (length list) < 8 then (addZero ((list) ++ [0])) else (list)
    
launch :: [String] -> IO (Int)
launch args =
    myWolfram rule start printlines window move
    where
        rule = getRule args;
        start = getParsedArgs args "--start" 0;
        printlines = getParsedArgs args "--lines" (-1);
        window = getParsedArgs args "--window" 80;
        move = getParsedArgs args "--move" 0;

isNumber :: String -> Bool
isNumber ('-':xs) = all isDigit xs
isNumber (x) = all isDigit x

myWolfram :: Int -> Int -> Int -> Int -> Int -> IO (Int)
myWolfram  rule start printlines window move | rule == 84 || start == 84 || printlines == 84 || window == -84 ||move == -84 = return(84)
myWolfram _ _ 0 _ _  = return(0)
myWolfram  rule start linesPrint window move = wLoop ([" "] ++ wCalculInfinite) (["*"] ++ wCalculInfinite) rule start linesPrint window move

wLoop :: [String] -> [String] -> Int -> Int -> Int -> Int -> Int -> IO (Int)
wLoop _ _ _ _ 0 _ _ = return (0)
wLoop left right rule start linesPrint window move |start /= 0 = do
    let createRight = loopCreate (addZero (intToBin rule)) right (head left) 0 
    let createLeft = loopCreateL (addZero (intToBin rule)) (left) (head right) 0 
    wLoop createLeft createRight rule (start - 1) linesPrint window move
wLoop left right rule start linesPrint window move = do
    displayLine window linesPrint (reverse (take ((window ) `div` 2) left))
    displayLine window linesPrint (take ((window ) `div` 2) right)
    putStr ("\n")
    let createRight = loopCreate (addZero (intToBin rule)) right (head left) 0 
    let createLeft = loopCreateL (addZero (intToBin rule)) (left) (head right) 0 
    wLoop createLeft createRight rule start (linesPrint - 1) window move

loopCreate :: [Int] -> [String] -> String -> Int -> [String]    
loopCreate binary (x:xs) other 0 =     (wMapCells binary other x  (head xs))  ++ (loopCreate binary (x:xs) other 1)
loopCreate binary (x:xs:xss) other 1 = (wMapCells binary  x    xs (head xss)) ++ (loopCreate binary (xs:xss) other 1)

loopCreateL :: [Int] -> [String] -> String -> Int -> [String]    
loopCreateL binary (x:xs) other 0 =     (wMapCells binary (head xs) x other )  ++ (loopCreateL binary (x:xs) other 1)
loopCreateL binary (x:xs:xss) other 1 = (wMapCells binary  (head xss) xs    x) ++ (loopCreateL binary (xs:xss) other 1)

wMapCells :: [Int] -> String -> String -> String -> [String]
wMapCells binary " " " " " " |  (head binary) == 1 = ["*"]
wMapCells binary " " " " "*" |  (head (drop 1 binary)) == 1 = ["*"]
wMapCells binary " " "*" " " |  (head (drop 2 binary)) == 1 = ["*"]
wMapCells binary " " "*" "*" |  (head (drop 3 binary)) == 1 = ["*"]
wMapCells binary "*" " " " " |  (head (drop 4 binary)) == 1 = ["*"]
wMapCells binary "*" " " "*" |  (head (drop 5 binary)) == 1 = ["*"]
wMapCells binary "*" "*" " " |  (head (drop 6 binary)) == 1 = ["*"]
wMapCells binary "*" "*" "*" |  (head (drop 7 binary)) == 1 = ["*"]
wMapCells _ _ _ _ = [" "]

wCalculInfinite ::  [String]
wCalculInfinite = [" "] ++ wCalculInfinite

displayLine :: Int -> Int  -> [String] -> IO ()
displayLine _ _ []  = return ()
displayLine linesPrint window (x:xs) = do
    putStr x
    displayLine linesPrint window xs

isNotArgs :: String -> Bool     
isNotArgs a 
    | (not (elem a ["--rule","--start","--lines","--window","--move"])) = True
    | otherwise                                                         = False

getParsedArgs :: [String] -> String -> Int -> Int  
getParsedArgs [] _ defaultValue = defaultValue
getParsedArgs (x:xs) name defaultValue
    | (x == name && (null xs) || ( isNotArgs x)) && not (isNumber x)    = -84
    | x == name && not (null xs) && (isNumber (head xs))                =  read (head xs) :: Int
    | otherwise                                                         = getParsedArgs  xs name defaultValue                                                                  

getRule :: [String] -> Int
getRule [] = -84
getRule (x:xs)
    | (x =="--rule" && (null xs) || ( isNotArgs x)) && not (isNumber x)                                                           = -84
    | x =="--rule" && not (null xs) && (isNumber (head xs)) && (read (head xs) :: Int) > 0 && (read (head xs) :: Int) < 257       = read (head xs) :: Int
    | otherwise                                                                                                                   = getRule xs