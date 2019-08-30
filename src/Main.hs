module Main (
  main,
) where

import Hasklog.Data
import Hasklog.Parser
import Hasklog.Interpreter

import Data.List (intercalate)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.Map as M
import List.Transformer (Step(..), next)
import System.Environment
import System.IO


main :: IO ()
main = interpret =<< interpreterSession <$> getArgs


check :: Either ParseError a -> a
check = either (error . show) id


readAndConsult :: String -> InterpreterT IO [HornClause]
readAndConsult file =
  do source <- lift $ readFile file
     check <$> consult program file source

interpreterSession :: [String] -> InterpreterT IO ()
interpreterSession files =
  do mapM_ readAndConsult files
     forever readEvalPrint


prompt :: String -> IO String
prompt q = do putStr q
              hFlush stdout
              getLine

promptQuery :: IO String
promptQuery = ("?- " ++) <$> prompt "?- "


-- | Prompt the user for a query and run it, reporting results as long as the
--   user requests them (or until they are exhausted).
readEvalPrint :: InterpreterT IO ()
readEvalPrint =
  do input <- lift promptQuery
     queryParseResult <- consult clause "(user input)" input
     case queryParseResult of
       Left error -> lift $ print error
       Right query -> showResults =<< next (resolve query)


-- | Format and print the next available unifier and prompt whether to
--   report another.
showResults :: Step (InterpreterT IO) Unifier -> InterpreterT IO ()
showResults Nil = lift $ putStrLn "false."
showResults (Cons unifier remaining)
  | M.null unifier = lift $ putStrLn "true."
  | otherwise = do
      response <- lift $ prompt (formatUnifier unifier ++ " ? ")
      when (response == ";") $ do
        lift $ putStrLn ""
        showResults =<< next remaining

  where          

    formatUnifier u = intercalate "\n" (map formatBinding (M.toList u))
    formatBinding (var, val) = var ++ " = " ++ concrete val
