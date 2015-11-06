
{-|
Module      : Qux.Exception
Description : Exceptions and utility functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Exceptions and utility functions
-}

module Qux.Exception (
    -- * Build exception
    BuildException(..),
) where

import Control.Exception

import Data.Typeable

import Language.Qux.Annotated.Exception
import Language.Qux.Annotated.Parser    (SourcePos)
import Language.Qux.Syntax


data BuildException = BuildException SourcePos String   -- ^ A generic build exception with a position and message.
                    | DuplicateModule SourcePos [Id]    -- ^ Indicates duplicate module found.
    deriving (Eq, Typeable)

instance CompilerException BuildException where
    pos (BuildException p _)    = p
    pos (DuplicateModule p _)   = p

    message (BuildException _ m) = m
    message (DuplicateModule _ id) = "duplicate module name \"" ++ qualify id ++ "\""

instance Exception BuildException

instance Show BuildException where
    show e = show (pos e) ++ ":\n" ++ message e

