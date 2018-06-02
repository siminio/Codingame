import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import Data.Char
import qualified Data.Map as Map  

getExtFile :: [Char] -> [Char]
getExtFile fname
    | elem '.' fname = map toLower $ last $ (splitOn "." fname)
    | otherwise = "noextension" 

getMimeType :: [Char] -> Map.Map [Char] [Char] -> [Char]
getMimeType ext mapExts
    | value == Nothing = "UNKNOWN"
    | otherwise = fromJust value
    where value = Map.lookup ext mapExts

main :: IO ()
main = do
    input_line <- getLine
    let n = read input_line :: Int -- Number of elements which make up the association table.
    input_line <- getLine
    let q = read input_line :: Int -- Number Q of file names to be analyzed.
    
    extensions <- replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let ext = map toLower $ input!!0 -- file extension
        let mt = input!!1 -- MIME type.
        return (ext, mt)

    let mapExts = Map.fromList extensions

    replicateM q $ do
        fname <- getLine
        let extFile = getExtFile fname
        putStrLn $ getMimeType extFile mapExts
        
    return ()