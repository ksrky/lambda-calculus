module UntypedSpec where

import Untyped.Parser

import System.Directory (getDirectoryContents)
import System.IO
import Test.Hspec

spec :: Spec
spec = do
        describe "Untyped" $ do
                paths <- getDirectoryContents "src/untyped"
                loop paths

loop :: [String] -> SpecWith ()
loop [] = return ()
loop (path : ps) = do
        it path $ do
                inp <- readFile path
                case parseTerm inp of
                        Left err -> putStrLn $ prettyError err
                        Right trm -> print $ eval trm
        loop ps
