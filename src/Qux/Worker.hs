
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

    -- ** Logging to a worker
    Priority(..),
    log, report,
) where

import Control.Monad.Except

import Pipes
import Prelude hiding (log)

import System.Exit
import System.IO


-- | Monad transformer that wraps a 'Producer' and an 'ExceptT'.
--   This provides an easy way to yield messages to the console and exit fast if an error occurs.
type WorkerT m = Producer (Priority, String) (ExceptT ExitCode m)

-- | Runs the worker printing out all yielded strings to the standard output.
runWorkerT :: MonadIO m => WorkerT m a -> m a
runWorkerT worker = runExceptT (runEffect $ for worker (liftIO . print)) >>=
    either (liftIO . exitWith) return
    where
        print (Error, message)  = hPutStrLn stderr message
        print (_, message)      = putStrLn message


-- | A message priority, in descending order of importance.
data Priority   = Debug
                | Info
                | Warn
                | Error
    deriving (Eq, Ord, Show)

-- | Logs the given message with the priority.
log :: Monad m => Priority -> String -> WorkerT m ()
log priority message = yield (priority, message)

-- | Logs all of the messages with the priority.
report :: (Monad m, Foldable f) => Priority -> f String -> WorkerT m ()
report priority messages = mapM_ (\message -> yield (priority, message)) messages

