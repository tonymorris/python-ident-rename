{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Python.Syntax.RenameOptions(
  RenameOptions(..)
, HasRenameOptions(..)
, identifierFromTo
, identifierFrom
, identifierTo
, renameFile
, optRenameOptions
, optRenameOptionsRun
) where

import Control.Category((.), id)
import Control.Lens
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.List.NonEmpty(NonEmpty, some1)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Text.IO(putStrLn, writeFile)
import Language.Python(ParseError, SrcInfo, Validation(Failure, Success), showModule, readModule)
import Language.Python.Syntax.Renamer(rename, renamerList1)
import Options.Applicative hiding (ParseError(..), Failure, Success)
import Prelude(Show(show))
import System.FilePath(FilePath)
import System.IO(stderr, hPutStrLn, IO)

data RenameOptions =
  RenameOptions
    FilePath
    (NonEmpty (String, String))
    (Maybe FilePath)
  deriving (Eq, Ord, Show)

class HasRenameOptions a where
  renameOptions ::
    Lens' a RenameOptions
  inputFilename ::
    Lens' a FilePath
  identifiersFromTo ::
    Lens' a (NonEmpty (String, String))
  inputFilename =
    renameOptions . inputFilename
  outputFilename ::
    Lens' a (Maybe FilePath)
  outputFilename =
    renameOptions . outputFilename

instance HasRenameOptions RenameOptions where
  renameOptions =
    id
  inputFilename f (RenameOptions i x o) =
    fmap (\i' -> RenameOptions i' x o) (f i)
  identifiersFromTo f (RenameOptions i x o) =
    fmap (\x' -> RenameOptions i x' o) (f x)
  outputFilename f (RenameOptions i x o) =
    fmap (\o' -> RenameOptions i x o') (f o)

identifierFromTo ::
  HasRenameOptions a =>
  Traversal' a (String, String)
identifierFromTo =
  identifiersFromTo . traverse

identifierFrom ::
  HasRenameOptions a =>
  Traversal' a String
identifierFrom =
  identifierFromTo . _1

identifierTo ::
  HasRenameOptions a =>
  Traversal' a String
identifierTo =
  identifierFromTo . _2

----

renameFile ::
  RenameOptions
  -> IO ()
renameFile p =
  do  k <- readModule @(ParseError SrcInfo) (p ^. inputFilename)
      case k of
        Failure e ->
          hPutStrLn stderr (show e)
        Success x ->
          let m = showModule (rename (renamerList1 (p ^. identifiersFromTo)) x)
          in  case p ^. outputFilename of
                Nothing ->
                  putStrLn m
                Just o ->
                  writeFile o m

optRenameOptions ::
  Parser RenameOptions
optRenameOptions =
  let optIdentifierInputFilename ::
        Parser FilePath
      optIdentifierInputFilename =
        strOption
          (
            short 'f' <>
            long "filename" <>
            help "python filename to rename"
          )
      optIdentifierFrom ::
        Parser String
      optIdentifierFrom =
        strOption
          (
            short 'x' <>
            long "from" <>
            help "identifier to rename"
          )
      optIdentifierTo ::
        Parser String
      optIdentifierTo =
        strOption
          (
            short 'y' <>
            long "to" <>
            help "name to rename identifier to"
          )
      optIdentifiersFromTo ::
        Parser (NonEmpty (String, String))
      optIdentifiersFromTo =
        some1 (liftA2 (,) optIdentifierFrom optIdentifierTo)
      optIdentifierOutputFilename ::
        Parser FilePath
      optIdentifierOutputFilename =
        strOption
          (
            short 'o' <>
            long "output" <>
            help "filename to output to"
          )
  in  RenameOptions <$>
      optIdentifierInputFilename <*>
      optIdentifiersFromTo <*>
      (optional optIdentifierOutputFilename)

optRenameOptionsRun ::
  IO RenameOptions
optRenameOptionsRun =
  execParser
    (info (optRenameOptions <**> helper) (
      fullDesc <>
      header "python-ident-rename <qfpl.io>"
    )
  )
