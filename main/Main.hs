
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Main where

import Options.Applicative

import              Qux.Commands
import qualified    Qux.Commands.Build      as Build
import qualified    Qux.Commands.Check      as Check
import qualified    Qux.Commands.Compile    as Compile
import qualified    Qux.Commands.Print      as Print
import qualified    Qux.Commands.Run        as Run


-- | Main.
main :: IO ()
main = customExecParser quxPrefs quxInfo >>= handle

handle :: Options -> IO ()
handle options = case argCommand options of
    Build   options -> Build.handle     options
    Check   options -> Check.handle     options
    Compile options -> Compile.handle   options
    Print   options -> Print.handle     options
    Run     options -> Run.handle       options

