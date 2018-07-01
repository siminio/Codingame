import System.IO
import Control.Monad

giveNearestTempFromZero :: [Int] -> [Int] -> String
giveNearestTempFromZero [] [] = "0"
giveNearestTempFromZero tempPos [] = "" ++ show (minimum tempPos)
giveNearestTempFromZero [] tempNeg = "-" ++ show (minimum (map abs tempNeg))
giveNearestTempFromZero tempPos tempNeg
    | minNeg < minPos = "-" ++ show (abs minNeg)
    | otherwise = show minPos
    where minNeg = minimum (map abs tempNeg)
          minPos = minimum tempPos


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input_line <- getLine
    let input = words input_line
    let temps = map read input :: [Int]
    let tempPos = filter (>=0) temps
    let tempNeg = filter (<0) temps
    putStrLn $ giveNearestTempFromZero tempPos tempNeg

    return ()