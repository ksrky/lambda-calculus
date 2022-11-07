module Main where

import Untyped.Eval (eval)
import Untyped.Parser (parseTerm, prettyError)
import Untyped.Syntax (emptyContext, printtm)

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
                fnames -> mapM_ processFile fnames

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> liftIO (process input) >> loop

processFile :: String -> IO ()
processFile n = do
        let path = "src/untyped/examples/" ++ n
        contents <- readFile path
        putStrLn $ "---------- " ++ path ++ " ----------"
        putStrLn $ "  " ++ contents
        putStr "> "
        process contents
        putStrLn ""

process :: String -> IO ()
process inp = case parseTerm inp of
        Left err -> putStrLn $ prettyError err
        Right term -> putStrLn $ printtm emptyContext (eval term)
