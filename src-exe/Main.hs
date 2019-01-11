{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Language.Python.Syntax.RenameOptions(optRenameOptionsRun, renameFile)
import System.IO(IO)

main ::
  IO ()
main =
  do  o <- optRenameOptionsRun
      renameFile o
