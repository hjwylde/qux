
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

    -- ** Logging messages
    Message, Priority(..),
    log, report,

    -- ** Manipulating messages
    requirePriority, prependPriority, prependTimestamp
) where

import Control.Monad.Except

import Data.List.Extra  (upper)
import Data.Time        (getZonedTime)

import              Pipes
import qualified    Pipes.Prelude   as Pipes
import              Prelude         hiding (log)

import System.Exit
import System.IO


-- | Monad transformer that wraps a 'Producer' and an 'ExceptT'.
--   This provides an easy way to yield messages to the console and exit fast if an error occurs.
type WorkerT m = Producer Message (ExceptT ExitCode m)

-- | Runs the worker printing out all yielded strings to the standard output.
runWorkerT :: MonadIO m => WorkerT m a -> m a
runWorkerT worker = runExceptT (runEffect $ for worker (liftIO . print)) >>=
    either (liftIO . exitWith) return
    where
        print (Error, message)  = hPutStrLn stderr message
        print (_, message)      = putStrLn message


-- | A string with a priority.
type Message = (Priority, String)

-- | A priority, in descending order of importance.
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


-- | A filter that only allows messages through with at least the given priority.
requirePriority :: Monad m => Priority -> Pipe Message Message m r
requirePriority min = Pipes.filter $ \(priority, _) -> priority >= min

-- | A filter that prepends the priority to the message.
--   The priority is prepended with right-padding.
prependPriority :: Monad m => Pipe Message Message m r
prependPriority = Pipes.map $ \(priority, message) ->
    let newMessage = unwords [
            upper (show priority) ++ (replicate (length (show Error) - length (show priority)) ' '),
            message
            ]
    in (priority, newMessage)

-- | A filter that prepends the timestamp to the message.
--   The timestamp is surrounded by brackets.
prependTimestamp :: MonadIO m => Pipe Message Message m r
prependTimestamp = do
    zonedTime <- liftIO getZonedTime

    Pipes.map $ \(priority, message) ->
        let newMessage = unwords [
                "[" ++ show zonedTime ++ "]",
                message
                ]
        in (priority, newMessage)
