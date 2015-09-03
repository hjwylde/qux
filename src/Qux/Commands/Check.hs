
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Check

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Check where

import qualified Qux.Commands.Build as Build


data Options = Options {
    argFilePaths :: [String]
    }

handle :: Options -> IO ()
handle options = Build.handle Build.Options {
    Build.optTypeCheck = True,
    Build.argFilePaths = argFilePaths options
    }

