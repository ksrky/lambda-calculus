module Main where

import Untyped.Evaluator (eval)
import Untyped.Parser (parseTerm, prettyError)
import Untyped.Syntax (printtm)

import Control.Monad.Trans (MonadIO (liftIO))
import System.Console.Haskeline (
        defaultSettings,
        getInputLine,
        outputStrLn,
        runInputT,
 )
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                fnames -> processFiles fnames

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> liftIO (process input) >> loop

processFiles :: [String] -> IO ()
processFiles [] = return ()
processFiles (n : ns) = do
        let path = "src/untyped/examples/" ++ n
        contents <- readFile path
        putStrLn $ "---------- " ++ path ++ " ----------"
        putStrLn $ "  " ++ contents
        putStr "> "
        process contents
        putStrLn ""
        processFiles ns

process :: String -> IO ()
process inp = case parseTerm inp of
        Left err -> putStrLn $ prettyError err
        Right trm -> print $ eval trm
