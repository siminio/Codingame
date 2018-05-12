import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    let horsepowersIO = replicateM n $ do
        input_line <- getLine
        let pi = read input_line :: Int
        return pi
    horsepowers <- horsepowersIO
    let hpSorted = sort horsepowers
    let spreads = zipWith (-)  (tail hpSorted) hpSorted
    let min = minimum spreads

    putStrLn (show min)
    return ()