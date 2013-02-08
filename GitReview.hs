{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where


import Data.Data
import System.Console.CmdArgs


data CliConfig = CliConfig { config :: String }
               deriving (Show, Data, Typeable)

gitReviewCliArgs :: CliConfig
gitReviewCliArgs =  CliConfig { config = def &= help "The configuration file." }
                 &= summary "GitReview v0"

main :: IO ()
main = print =<< cmdArgs gitReviewCliArgs

