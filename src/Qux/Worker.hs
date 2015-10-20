
{-|
Module      : Qux.Worker
Description : Monad transformer wrapping a 'Producer' and 'ExceptT'.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Monad transformer wrapping a 'Producer' and an 'ExceptT'.
-}

module Qux.Worker (
    -- * WorkerT
    WorkerT,
    runWorkerT,
) where

import Control.Monad.Except

import Pipes

import System.Exit


-- | Monad transformer that wraps a 'Producer' and an 'ExceptT'.
--   This provides an easy way to yield messages to the console and exit fast if an error occurs.
type WorkerT m = Producer String (ExceptT ExitCode m)

-- | Runs the worker printing out all yielded strings to the standard output.
runWorkerT :: MonadIO m => WorkerT m a -> m a
runWorkerT worker = runExceptT (runEffect $ for worker (liftIO . putStrLn)) >>=
    either (liftIO . exitWith) return

