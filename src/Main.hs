-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
  main,
) where

import Prolog.Compiler
import Prolog.Data
import Prolog.Parser
import Prolog.Interpreter

import Data.List (intersperse)
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (ParseError, SourceName)
import qualified Data.Map as M
import System.Environment
import System.Exit
import System.IO


main =
  do args <- getArgs
     interpret $ interpreterSession args

-- | Print a usage message and exit.
usage =
  do name <- getProgName
     let msg = "Usage: " ++ name ++ " [INPUT...]"
     putStrLn msg
     exitFailure


-- | Run a parser and either return its result (if successful) or report the
--   parse error to the user.
tryParse :: (Monad m, Functor m) => PrologParser m a -> SourceName -> String -> m a
tryParse p src input = check <$> parse p src input

check :: Either ParseError a -> a
check = either (error . show) id


readAndConsult :: String -> InterpreterT IO [HornClause]
readAndConsult file =
  do source <- liftIO $ readFile file
     rslt <- consult program file source
     return $ check rslt

interpreterSession :: [String] -> InterpreterT IO ()
interpreterSession files =
  do mapM readAndConsult files
     forever prompt


-- | Prompt the user for a query and run it, reporting results as long as the
--   user requests them (or until they are exhausted).
prompt =
  do liftIO $ putStr "?- "
     liftIO $ hFlush stdout
     input <- liftIO $ getLine
     query <- check <$> consult clause "(user input)" ("?- " ++ input)
     resolution <- resolve query
     liftIO $ showResults resolution

   where

     -- | Format and print the next available unifier and prompt whether to
     --   report another.
     showResults []     = putStrLn "false."
     showResults (u:us)
       | M.null u  = putStrLn "true."
       | otherwise =
           do putStrLn (formatUnifier u)
              response <- getLine
              if response == ";"
                then showResults us
                else return ()

     formatUnifier u = concat (intersperse "\n" (map formatBinding (M.toList u)))
       where
         formatBinding (var,val) = var ++ " = " ++ concrete val
