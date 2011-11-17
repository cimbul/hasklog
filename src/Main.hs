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

import Prolog.Parser
import Prolog.Interpreter (resolve, unify)

import Data.List (intersperse)
import Control.Monad
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.Map as M
import System.Environment
import System.Exit
import System.IO


main =
  do -- Check usage
     args <- getArgs
     when (length args /= 1) usage

     -- Consult source file given as first argument
     let sourceName = args !! 0
     source <- readFile sourceName
     let prog = tryParse program sourceName source

     -- Start REPL
     forever (prompt prog)

-- | Print a usage message and exit.
usage =
  do name <- getProgName
     let msg = "Usage: " ++ name ++ " INPUT"
     putStrLn msg
     exitFailure

-- | Run a parser and either return its result (if successful) or report the
--   parse error to the user.
tryParse p src input = check $ parseFully p src input
  where
    check = either (error . show) id


-- | Prompt the user for a query and run it, reporting results as long as the
--   user requests them (or until they are exhausted).
prompt prog =
  do putStr "?- "
     hFlush stdout
     input <- getLine
     let q = tryParse query "(user input)" input
     showResults (resolve prog q)

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
         formatBinding (var,val) = var ++ " = " ++ concreteSyntax val
