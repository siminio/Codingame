import System.IO
import Control.Monad

giveDirectionVertical :: Int -> String
giveDirectionVertical dy
    | dy > 0 = "N"
    | dy < 0 = "S"
    | otherwise = ""

giveDirectionHorizontal :: Int -> String
giveDirectionHorizontal dx
    | dx > 0 = "W"
    | dx < 0 = "E"
    |otherwise = ""

giveDirection :: Int -> Int -> String  
giveDirection dx dy = giveDirectionVertical dy ++ giveDirectionHorizontal dx 

move :: Int -> Int -> Int -> Int -> (Int, Int)
move tx ty dx dy = (tx - signum dx, ty - signum dy)
    
main :: IO ()
main = do  
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position
    loop lx ly tx ty



loop :: Int -> Int -> Int -> Int -> IO ()  
loop lx ly tx ty = do  
    input_line <- getLine
    let energy = read input_line :: Int -- The level of Thor's remaining energy, representing the number of moves he can still make.
    let dx = tx - lx
    let dy = ty - ly
    let direction = giveDirection dx dy
    let (ntx,nty) = move tx ty dx dy

    putStrLn  $ direction

    loop lx ly ntx nty