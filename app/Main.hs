
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main
) where

import Options.Applicative

import Pipes
import Prelude hiding (log)

import              Qux.Commands
import qualified    Qux.Commands.Build          as Build
import qualified    Qux.Commands.Check          as Check
import qualified    Qux.Commands.Compile        as Compile
import qualified    Qux.Commands.Dependencies   as Dependencies
import qualified    Qux.Commands.Print          as Print
import              Qux.Worker


main :: IO ()
main = customExecParser quxPrefs quxInfo >>= handle

handle :: Options -> IO ()
handle options = runWorkerT $ (header >> worker) >-> quiet >-> verbose
    where
        header = log Debug $ show options

        worker = case argCommand options of
            Build           options -> Build.handle         options
            Check           options -> Check.handle         options
            Compile         options -> Compile.handle       options
            Dependencies    options -> Dependencies.handle  options
            Print           options -> Print.handle         options

        quiet
            | optQuiet options  = requirePriority Error
            | otherwise         = cat

        verbose
            | optVerbose options    = prependPriority >-> prependTimestamp
            | otherwise             = requirePriority Info

