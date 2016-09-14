{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Main (
    -- * Main
    main
) where

import Options.Applicative

import Pipes
import Prelude hiding (log)

import qualified Qux.Command.Build        as Build
import qualified Qux.Command.Check        as Check
import qualified Qux.Command.Compile      as Compile
import qualified Qux.Command.Dependencies as Dependencies
import qualified Qux.Command.Print        as Print
import           Qux.Options
import           Qux.Worker

main :: IO ()
main = customExecParser quxPrefs quxInfo >>= handle

handle :: Options -> IO ()
handle options = runWorkerT $ (logOptions options >> worker) >-> quietFilter options >-> verboseFilter options
    where
        worker = case argCommand options of
            Build options           -> Build.handle options
            Check options           -> Check.handle options
            Compile options         -> Compile.handle options
            Dependencies options    -> Dependencies.handle options
            Print options           -> Print.handle options

logOptions :: Options -> WorkerT IO ()
logOptions = log Debug . show

quietFilter :: Monad m => Options -> Pipe Message Message m r
quietFilter options
    | optQuiet options  = requirePriority Error
    | otherwise         = cat

verboseFilter :: MonadIO m => Options -> Pipe Message Message m r
verboseFilter options
    | optVerbose options    = prependPriority >-> prependTimestamp
    | otherwise             = requirePriority Info
