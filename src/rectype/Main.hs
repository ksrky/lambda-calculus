module Main where

import Evaluator
import Parser
import Syntax

import Control.Exception.Safe (MonadThrow)
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
        let path = "src/rectype/examples/" ++ n
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

processCommand :: (MonadThrow m, MonadIO m) => Command -> StateT Context m ()
processCommand cmd = case cmd of
        Eval t -> do
                ctx <- get
                tyT <- typeof ctx t
                let t' = eval ctx t
                -- t' <- liftIO $ evalIO ctx t
                liftIO $ do
                        putStrLn $ "  " ++ printtm ctx False t
                        putStr $ "> " ++ printtm ctx False t'
                        putStr " : "
                        putStrLn $ printty ctx False tyT
        Bind x bind -> do
                ctx <- get
                bind' <- checkBinding ctx bind
                modify $ addbinding x bind'
