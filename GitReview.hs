{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where


import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Char8 as BS
import qualified Data.Configurator as C
import           Data.Configurator.Types
import           Data.Data
import qualified Data.Text as T
import           Data.Time
import           Github.Api
import           Github.Data
import           Github.Review
import           Github.Review.Format
import           System.Console.CmdArgs
import           System.Environment
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
                     { samplePeriod   :: Int
                     , sampleN        :: Int
                     , emailAddresses :: [NameAddr]
                     , ghAccount      :: GithubAccount
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

-- Getting the commit

getGithubCommit :: GitReviewConfig -> GithubAuth -> GithubInteraction RepoCommit
getGithubCommit GitReviewConfig{..} auth = do
        limit <-  liftIO
              $   offsetByDays (fromIntegral samplePeriod)
              <$> getCurrentTime
        cs    <- mapM (`getCommits` sampleN) =<< getAccountRepos ghAccount
        fmapLT (UserError . T.unpack)
            . pickRandom
            . getAfterOrMinimum (getCommitDate . snd) limit sampleN
            . sortByCommitDate
            $ concat cs
        where getCommits = getAllRepoCommits' (Just auth)

-- Utilities

ghError :: Show a => Either a b -> Either Error b
ghError = fmapL (UserError . show)

ghErrorT :: Show a => EitherT a IO b -> GithubInteraction b
ghErrorT = fmapLT (UserError . show)

ghIO :: IO a -> GithubInteraction a
ghIO = ghErrorT . tryIO

newMsg :: String -> GithubInteraction a -> GithubInteraction a
newMsg msg = fmapLT (const (UserError msg))

-- Main

main :: IO ()
main = do
    result <- runGithubInteraction $ do
        auth <- newMsg "Missing authentication environment variables \
                       \(GITHUB_USER and GITHUB_PASSWD)."
                (GithubBasicAuth <$> ghIO (BS.pack <$> getEnv "GITHUB_USER")
                                 <*> ghIO (BS.pack <$> getEnv "GITHUB_PASSWD"))
        cfg  <-  getReviewConfig
             =<< ghIO (   C.load
                      =<< (:[]) . C.Required . config
                      <$> cmdArgs gitReviewCliArgs)

        rc@(r, c) <- getGithubCommit cfg auth
        liftIO . putStrLn . T.unpack $ formatCommitText r c

        return rc

    return ()

