import System.IO
import Control.Monad
import Data.List
import Data.Ord

giveClosestTempFromZero :: [Int] -> Int
giveClosestTempFromZero [] = 0
giveClosestTempFromZero temps = minimumBy (comparing cmp) temps

cmp x = (x^2) - x

main :: IO ()
main = do
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input_line <- getLine
    let input = words input_line
    let temps = map read input :: [Int]
    putStrLn $ show $ giveClosestTempFromZero temps