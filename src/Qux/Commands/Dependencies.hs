
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

import Data.List.Extra (nubOrd, sort)

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import Pipes

import qualified    Qux.Commands.Build as Build
import              Qux.Worker


-- | Dependencies options.
data Options = Options {
    argFilePaths :: [FilePath]
    }
    deriving (Eq, Show)


-- | Prints out the file dependencies according to the options.
handle :: Options -> IO ()
handle options = runWorkerT $ Build.parseAll (argFilePaths options) >>= dependencies


dependencies :: [Program SourcePos] -> WorkerT IO ()
dependencies programs = each $ nubOrd (sort [simp (qualify id) |
    (Program _ _ decls) <- programs,
    (ImportDecl _ id) <- decls
    ])

