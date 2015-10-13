
{-|
Module      : Qux.Commands.Check

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Commands.Check where

import qualified Qux.Commands.Build as Build


data Options = Options {
    argFilePaths :: [FilePath]
    }
    deriving (Eq, Show)


handle :: Options -> IO ()
handle options = Build.handle $ buildOptions options

buildOptions :: Options -> Build.Options
buildOptions options = Build.defaultOptions {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }

