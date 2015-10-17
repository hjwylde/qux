
{-|
Module      : Qux.Worker

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Qux.Worker where

import Control.Monad.Except

import Pipes

import System.Exit


type WorkerT m = Producer String (ExceptT ExitCode m)

runWorkerT :: MonadIO m => WorkerT m a -> m a
runWorkerT worker = runExceptT (runEffect $ for worker (liftIO . putStrLn)) >>=
    either (liftIO . exitWith) return

