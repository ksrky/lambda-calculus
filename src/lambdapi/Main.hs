{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Safe (SomeException, catch)
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

import LambdaPi.Eval
import LambdaPi.Lexer (alexScanTokens)
import LambdaPi.Parser (parse)
import LambdaPi.Syntax (
        Command (..),
        Context,
        addBinding,
        emptyContext,
        printbind,
        printtm,
 )

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> runInputT defaultSettings (repl emptyContext)
                fnames -> mapM_ processFile fnames

repl :: Context -> InputT IO ()
repl ctx = do
        minp <- getInputLine ">> "
        case minp of
                Nothing -> outputStrLn "Goodbye."
                Just "" -> outputStrLn "Goodbye."
                Just inp -> do
                        ctx' <- liftIO $ catch (process inp ctx) (\(e :: SomeException) -> print e >> return ctx)
                        repl ctx'

processFile :: String -> IO ()
processFile n = do
        let path = "src/lambdapi/examples/" ++ n
        inp <- readFile path
        putStrLn $ "---------- " ++ path ++ " ----------"
        ctx <- process inp emptyContext
        runInputT defaultSettings (repl ctx)

process :: String -> Context -> IO Context
process inp ctx = do
        out <- parse (alexScanTokens inp)
        cmds <- out ctx
        foldM processCommand ctx cmds

processCommand :: (MonadFail m, MonadIO m) => Context -> Command -> m Context
processCommand ctx (Eval t) = do
        _ <- typeof ctx t
        let t' = eval ctx t
        liftIO $ putStrLn $ printtm ctx t'
        return ctx
processCommand ctx (Bind x bind) = do
        bind' <- checkBinding ctx bind
        let bind'' = evalBinding ctx bind'
        liftIO $ putStrLn $ printbind ctx (x, bind'')
        return $ addBinding x bind'' ctx