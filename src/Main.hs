import VoorbeeldModule (hoi)

import System.Environment (getArgs)

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          putStrLn hoi
          putStrLn contents