module Main (
  main,
) where

import Prolog.Data
import Prolog.Parser
import Prolog.Interpreter

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
main =
  do args <- getArgs
     interpret $ interpreterSession args


check :: Either ParseError a -> a
check = either (error . show) id


readAndConsult :: String -> InterpreterT IO [HornClause]
readAndConsult file =
  do source <- lift $ readFile file
     rslt <- consult program file source
     return $ check rslt

interpreterSession :: [String] -> InterpreterT IO ()
interpreterSession files =
  do mapM_ readAndConsult files
     forever readEvalPrint


prompt :: String -> IO String
prompt q = do putStr q
              hFlush stdout
              getLine

promptQuery :: IO String
promptQuery = do input <- prompt "?- "
                 return ("?- " ++ input)


-- | Prompt the user for a query and run it, reporting results as long as the
--   user requests them (or until they are exhausted).
readEvalPrint :: InterpreterT IO ()
readEvalPrint =
  do input <- lift promptQuery
     query <- check <$> consult clause "(user input)" input
     showResults =<< next (resolve query)

  where

    -- | Format and print the next available unifier and prompt whether to
    --   report another.
    showResults :: Step (InterpreterT IO) Unifier -> InterpreterT IO ()
    showResults Nil = lift $ putStrLn "false."
    showResults (Cons u us)
      | M.null u = lift $ putStrLn "true."
      | otherwise =
          do response <- lift $ prompt (formatUnifier u ++ " ? ")
             when (response == ";") $
               do lift $ putStrLn ""
                  showResults =<< next us

    formatUnifier u = intercalate "\n" (map formatBinding (M.toList u))
      where
        formatBinding (var,val) = var ++ " = " ++ concrete val
