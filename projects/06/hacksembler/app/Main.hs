{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
-- import Options.Applicative
import qualified Paths_hacksembler

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_hacksembler.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
        <$> argument str (metavar "FILE")
    )
    empty
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
