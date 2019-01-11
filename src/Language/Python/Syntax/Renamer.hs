{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Python.Syntax.Renamer(
  Renamer(..)
, renamerPair
, renamerList
, renamerList1
, identifierValues
, rename
) where

import Control.Category((.), id)
import Control.Lens
import Data.Eq(Eq((==)))
import Data.Foldable(foldMap)
import Data.List.NonEmpty(NonEmpty)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Semigroup(Semigroup((<>)))
import Data.Semigroup.Foldable(foldMap1)
import Data.String(String)
import Language.Python(HasStatements, HasIdents(_Idents), _Statements, _Exprs, _Ident, identValue)


newtype Renamer =
  Renamer
    (String -> String)

instance Renamer ~ a =>
  Rewrapped Renamer a

instance Wrapped Renamer where
  type Unwrapped Renamer =
    String
    -> String
  _Wrapped' =
    iso
      (\(Renamer x) -> x)
      Renamer

instance Semigroup Renamer where
  Renamer f <> Renamer g =
    Renamer (f . g)

instance Monoid Renamer where
  mappend =
    (<>)
  mempty =
    Renamer id

renamerPair ::
  (String, String)
  -> Renamer
renamerPair (s1, s2) =
  Renamer (
    \s ->
      if s == s1
        then
          s2
        else
          s
  )

renamerList ::
  [(String, String)]
  -> Renamer
renamerList =
  foldMap renamerPair

renamerList1 ::
  NonEmpty (String, String)
  -> Renamer
renamerList1 =
  foldMap1 renamerPair

identifierValues ::
  HasStatements s =>
  Traversal (s v a) (s '[] a) String String
identifierValues =
  _Statements . _Exprs . _Ident . identValue

rename ::
  HasIdents s =>
  Renamer
  -> s v a
  -> s '[] a
rename (Renamer r) =
  over (_Idents . identValue) r
