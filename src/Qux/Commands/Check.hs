
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
    argFilePath :: String
    }

handle :: Options -> IO ()
handle options = Build.handle Build.Options {
    Build.argFilePath = argFilePath options,
    Build.optTypeCheck = True
    }

