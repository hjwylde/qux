
{-|
Module      : Qux.Commands.Dependencies

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Dependencies where

import Data.List.Extra (nubOrd, sort)

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import Pipes

import qualified    Qux.Commands.Build as Build
import              Qux.Worker


data Options = Options {
    argFilePaths :: [FilePath]
    }
    deriving (Eq, Show)


handle :: Options -> IO ()
handle options = runWorkerT $ Build.parseAll (argFilePaths options) >>= dependencies

dependencies :: [Program SourcePos] -> WorkerT IO ()
dependencies programs = each $ nubOrd (sort [simp (qualify id) |
    (Program _ _ decls) <- programs,
    (ImportDecl _ id) <- decls
    ])

