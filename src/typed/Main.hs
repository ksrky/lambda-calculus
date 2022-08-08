module Main where

import Typed.Evaluator (eval, typeof)
import Typed.Parser (pCommands, prettyError)
import Typed.Syntax (CT, Command (..), Context, addbinding, emptyContext, evalCT, printtm, printty)

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Trans (MonadIO (liftIO))
import System.Console.Haskeline (
        InputT,
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
repl = runInputT defaultSettings (loop emptyContext)
    where
        loop :: Context -> InputT IO ()
        loop ctx = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just inp -> do
                                ctx' <- liftIO $ process inp ctx
                                loop ctx'

processFile :: String -> IO ()
processFile n = do
        let path = "src/typed/examples/" ++ n
        contents <- readFile path
        putStrLn $ "---------- " ++ path ++ " ----------"
        process contents emptyContext
        putStrLn ""

process :: String -> Context -> IO Context
process inp ctx = case pCommands inp of
        Left err -> putStrLn (prettyError err) >> return ctx
        Right cmds -> do
                ctx' <- mapM processCommand cmds `execStateT` ctx
                return ctx

processCommand :: (MonadThrow m, MonadIO m) => Command -> CT m ()
processCommand cmd = case cmd of
        Eval t -> do
                tyT <- evalCT $ typeof t
                let t' = eval t
                ctx <- get
                liftIO $ do
                        putStrLn $ "  " ++ printtm ctx t
                        putStr $ "> " ++ printtm ctx t'
                        putStr " : "
                        putStrLn $ printty tyT
        Bind x bind -> do
                addbinding x bind
