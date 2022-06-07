module Main where

import Typed.Evaluator (eval)
import Typed.Parser (parseTerm, prettyError)
import Typed.Syntax (printtm)

import Control.Monad.Trans (MonadIO (liftIO))
import System.Console.Haskeline (
        defaultSettings,
        getInputLine,
        outputStrLn,
        runInputT,
 )

main :: IO ()
main = repl

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> liftIO (process input) >> loop

process :: String -> IO ()
process inp = case parseTerm inp of
        Left err -> putStrLn $ prettyError err
        Right trm -> print $ eval trm
