
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Check

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Check where

import Control.Monad.Except

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Syntax

import qualified Qux.Commands.Build as Build


data Options = Options {
    argFilePaths :: [String]
    }

handle :: Options -> IO ()
handle options = Build.handle $ buildOptions options

check :: Options -> Program SourcePos -> Except String ()
check options = Build.build $ buildOptions options

buildOptions :: Options -> Build.Options
buildOptions options = Build.Options {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }

