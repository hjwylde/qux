{-|
Module      : Qux.BuildSteps
Description : Build step utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Build step utilities.
-}

module Qux.BuildSteps (
    -- * Parsing
    parse, parseAll,
) where

import Control.Monad.Except
import Control.Monad.Extra

import           Language.Qux.Annotated.Parser hiding (parse)
import qualified Language.Qux.Annotated.Parser as Parser
import           Language.Qux.Annotated.Syntax

import Prelude hiding (log)

import Qux.Worker

import System.Directory.Extra
import System.Exit

-- | Parses the file.
--   Returns the program if successful or yields the error message(s).
parse :: FilePath -> WorkerT IO (Program SourcePos)
parse filePath = do
    unlessM (liftIO $ doesFileExist filePath) $ do
        log Error $ "Cannot find file " ++ filePath
        throwError $ ExitFailure 1

    contents <- liftIO $ readFile filePath

    case runExcept (Parser.parse program filePath contents) of
        Left error      -> log Error (show error) >> throwError (ExitFailure 1)
        Right program   -> return program

-- | Parses the files.
--   Returns the programs if successful or yields the error message(s).
parseAll :: [FilePath] -> WorkerT IO [Program SourcePos]
parseAll = mapM parse
