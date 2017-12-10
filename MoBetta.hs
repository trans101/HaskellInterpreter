module Main where

import System.Environment
import System.IO 
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Error

-- construct abstract syntax trees from source code
import MoBettaParser 
-- construct computations from abstract syntax trees
import MoBettaEngine

main = do
    args <- getArgs
    let fileName = head args
    putStrLn $ "MoBetta running file: " ++ fileName ++ " ...\n"
    -- open file named by initial argument
    handle <- openFile fileName ReadMode
    -- get entire contents of file lazily
    contents <- hGetContents handle 
    let pr = mbparse fileName contents
    case pr of
      Right prog -> evalStateT (makeProgram prog) emptyEnv
      Left err  -> putStrLn $ parseErrorPretty' contents err
    hClose handle
