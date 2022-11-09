module Main where

import SystemF.Eval (eval, typeof)
import SystemF.Parser (pCommands, prettyError)
import SystemF.Syntax (
        Command (..),
        Context,
        addBinding,
        emptyContext,
        printtm,
        printty,
 )

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
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
        let path = "src/systemf/examples/" ++ n
        contents <- readFile path
        putStrLn $ "---------- " ++ path ++ " ----------"
        process contents emptyContext
        putStrLn ""

process :: String -> Context -> IO Context
process inp ctx = case pCommands inp of
        Left err -> putStrLn (prettyError err) >> return ctx
        Right cmds -> foldM processCommand ctx cmds

processCommand :: (MonadFail m, MonadIO m) => Context -> Command -> m Context
processCommand ctx (Eval t) = do
        tyT <- typeof ctx t
        let t' = eval ctx t
        liftIO $ do
                putStrLn $ "  " ++ printtm ctx t
                putStr $ "> " ++ printtm ctx t'
                putStr " : "
                putStrLn $ printty ctx tyT
        return ctx
processCommand ctx (Bind x bind) = return $ addBinding x bind ctx
