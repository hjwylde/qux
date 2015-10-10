
{-|
Module      : Qux.Commands.Check

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Check where

import Control.Monad.Except

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import qualified Qux.Commands.Build as Build


data Options = Options {
    argFilePaths :: [FilePath]
    }

handle :: Options -> IO ()
handle options = Build.handle $ buildOptions options

check :: Options -> Program SourcePos -> ExceptT String IO ()
check options = Build.build $ buildOptions options

buildOptions :: Options -> Build.Options
buildOptions options = Build.defaultOptions {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }

