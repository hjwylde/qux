{-|
Module      : Qux.Commands.Dependencies
Description : Options and handler for the dependencies subcommand.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the dependencies subcommand.
-}

module Qux.Commands.Dependencies (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.IO.Class

import Data.List.Extra (nubOrd, sort)

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import Prelude hiding (log)

import qualified    Qux.Commands.Build as Build
import              Qux.Worker


-- | Dependencies options.
data Options = Options {
    argFilePaths :: [FilePath] -- ^ The files to read the dependencies from.
    }
    deriving (Eq, Show)


-- | Prints out the file dependencies according to the options.
handle :: Options -> WorkerT IO ()
handle options = do
    log Debug "Parsing ..."
    Build.parseAll (argFilePaths options) >>= dependencies


dependencies :: [Program SourcePos] -> WorkerT IO ()
dependencies programs = liftIO $ mapM_ putStrLn (nubOrd $ sort [simp $ qualify id |
    (Program _ _ decls) <- programs,
    (ImportDecl _ id)   <- decls
    ])
