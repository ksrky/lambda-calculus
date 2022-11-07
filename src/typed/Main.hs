module Main where

import Typed.Eval (eval, typeof)
import Typed.Parser (pCommands, prettyError)
import Typed.Syntax (
        Command (..),
        Context,
        addbinding,
        emptyContext,
        printtm,
        printty,
 )

import Control.Exception.Safe (MonadThrow)
import Control.Monad.State (
        MonadIO (..),
        MonadState (get),
        StateT,
        execStateT,
        modify,
 )
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

processCommand :: (MonadThrow m, MonadIO m) => Command -> StateT Context m ()
processCommand cmd = case cmd of
        Eval t -> do
                ctx <- get
                tyT <- typeof ctx t
                let t' = eval t
                liftIO $ do
                        putStrLn $ "  " ++ printtm ctx t
                        putStr $ "> " ++ printtm ctx t'
                        putStr " : "
                        putStrLn $ printty tyT
        Bind x bind -> modify $ addbinding x bind
