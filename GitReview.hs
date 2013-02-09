{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where


import           Control.Applicative
import           Control.Error
import qualified Data.Configurator as C
import           Data.Configurator.Types
import           Data.Data
import           Github.Data
import           Github.Review
import           System.Console.CmdArgs
import           Text.Parsec (parse)
import           Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..), address_list)


-- Command-line parsing.

data CliConfig = CliConfig { config :: String }
               deriving (Show, Data, Typeable)

gitReviewCliArgs :: CliConfig
gitReviewCliArgs =  CliConfig { config = def &= help "The configuration file." }
                 &= summary "GitReview v0"

-- Configuration file.

data GitReviewConfig = GitReviewConfig
                     { samplePeriod :: Int
                     , sampleN      :: Int
                     , emailAddresses :: [NameAddr]
                     , ghAccount :: GithubAccount
                     } deriving (Show)

getReviewConfig :: Config -> GithubInteraction GitReviewConfig
getReviewConfig cfg =
        GitReviewConfig <$> liftIO (C.lookupDefault 1  cfg "sample.period")
                        <*> liftIO (C.lookupDefault 10 cfg "sample.n")
                        <*> getEmailAddresses cfg
                        <*> getGithubAccount cfg

getEmailAddresses :: Config -> GithubInteraction [NameAddr]
getEmailAddresses cfg = do
        addrStr <- hoistGH (noteError <$> lookupEmail)
        hoistEither $ parseAddr addrStr
        where noteError   = note (UserError "No target.email in config.")
              lookupEmail = C.lookup cfg "target.email"
              parseAddr   = ghError . parse address_list ""

getGithubAccount :: Config -> GithubInteraction GithubAccount
getGithubAccount cfg =
        hoistGH (   note (UserError "You must set either the organization or account.")
                .   listToMaybe
                .   catMaybes
                <$> sequence [ fmap GithubOrgName  <$> C.lookup cfg "github.organization"
                             , fmap GithubUserName <$> C.lookup cfg "github.account"
                             ])

-- Utilities

ghError :: Show a => Either a b -> Either Error b
ghError = fmapL (UserError . show)

ghErrorT :: Show a => EitherT a IO b -> GithubInteraction b
ghErrorT = fmapLT (UserError . show)

-- Main

main :: IO ()
main = do
    result <- runGithubInteraction $ do
        cfg <-  getReviewConfig
            =<< (ghErrorT . tryIO $   C.load
                                  =<< (:[]) . C.Required . config
                                  <$> cmdArgs gitReviewCliArgs)
        liftIO $ print cfg
    print result

