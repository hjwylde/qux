
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

check :: Program SourcePos -> Except String ()
check = Build.build $ buildOptions undefined

buildOptions :: Options -> Build.Options
buildOptions options = Build.Options {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }

