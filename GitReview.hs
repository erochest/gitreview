{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where


import           Control.Applicative
import           Control.Error
import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Configurator as C
import           Data.Configurator.Types
import           Data.Data
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Github.Api
import           Github.Data
import           Github.Review
import           Github.Review.Format
import           Network (PortID(..))
import qualified Network.Mail.Mime as Mime
import           Network.Mail.SMTP
import           Network.Mail.SMTP.TLS
import           Network.Socket (HostName, PortNumber(..))
import           System.Console.CmdArgs
import           System.Environment
import           System.Exit
import           System.Locale (defaultTimeLocale)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Parsec (parse)
import           Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..), address_list, name_addr)


-- Command-line parsing.

data CliConfig = CliConfig { config :: String }
               deriving (Show, Data, Typeable)

gitReviewCliArgs :: CliConfig
gitReviewCliArgs =  CliConfig { config = def &= help "The configuration file." }
                 &= summary "GitReview v0"

-- Configuration file.

data GitReviewConfig = GitReviewConfig
                     { samplePeriod   :: !Int
                     , sampleN        :: !Int
                     , ghAccount      :: !GithubAccount
                     , emailAddresses :: ![NameAddr]
                     , smtpHost       :: !HostName
                     , smtpPort       :: !Int
                     , smtpUser       :: !UserName
                     , smtpPassword   :: !Password
                     } deriving (Show)

getReviewConfig :: Config -> GithubInteraction GitReviewConfig
getReviewConfig cfg =
        GitReviewConfig <$> lookupIO 1  "sample.period"
                        <*> lookupIO 10 "sample.n"
                        <*> getGithubAccount cfg
                        <*> getEmailAddresses cfg
                        <*> lookupT "Missing config: smtp.host"     "smtp.host"
                        <*> lookupT "Missing config: smtp.port"     "smtp.port"
                        <*> lookupT "Missing config: smtp.user"     "smtp.user"
                        <*> lookupT "Missing config: smtp.password" "smtp.password"
        where lookupIO def = liftIO . C.lookupDefault def cfg
              lookupT msg name =    ghIO (C.lookup cfg name)
                               >>=  noteT (UserError msg) . hoistMaybe

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

-- Sending mail
getDateStr :: GithubInteraction T.Text
getDateStr = do
        t <- ghIO (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
        return . T.pack $ formatTime defaultTimeLocale "%c" t

wrapAndParse :: String -> GithubInteraction Address
wrapAndParse user = toAddress <$> ( hoistEither
                                  . ghError
                                  . parse name_addr ""
                                  $ "<" <> user <> ">")

toAddress :: NameAddr -> Address
toAddress NameAddr{..} =
        Address (T.pack <$> nameAddr_name) $ T.pack nameAddr_addr

sendCommit :: GitReviewConfig -> Repo -> Commit -> GithubInteraction ()
sendCommit GitReviewConfig{..} r c = do
    dateStr <- getDateStr
    from    <- wrapAndParse smtpUser
    ghIO . forM_ emailAddresses $ \to ->
        let to' = toAddress to
        in  commentEmail to' from ("Commit to review for " <> dateStr) r c >>=
            sendMailTls' smtpHost smtpPort smtpUser smtpPassword 

sendError :: GitReviewConfig -> Error -> GithubInteraction ()
sendError GitReviewConfig{..} err = do
    dateStr <- getDateStr
    from    <- wrapAndParse smtpUser
    ghIO . forM_ emailAddresses $ \to ->
        let (asText, asHtml) = formatError err
        in  Mime.simpleMail (toAddress to) from
                       ("ERROR: Retreiving commits to review for " <> dateStr)
                       (TL.fromStrict asText)
                       (renderHtml asHtml)
                       [] >>=
            sendMailTls' smtpHost smtpPort smtpUser smtpPassword

sendResults :: GitReviewConfig -> Either Error (Repo, Commit)
            -> GithubInteraction ()
sendResults cfg (Right (r, c)) = sendCommit cfg r c
sendResults cfg (Left err)     = sendError cfg err

-- Main

main :: IO ()
main = do
    retCode <- runGithubInteraction $ do
        cfg  <-  getReviewConfig
             =<< ghIO (   C.load
                      =<< (:[]) . C.Required . config
                      <$> cmdArgs gitReviewCliArgs)

        result <- ghIO . runGithubInteraction $ do
            auth <- newMsg "Missing authentication environment variables \
                           \(GITHUB_USER and GITHUB_PASSWD)."
                    (GithubBasicAuth <$> ghIO (BS.pack <$> getEnv "GITHUB_USER")
                                     <*> ghIO (BS.pack <$> getEnv "GITHUB_PASSWD"))

            getGithubCommit cfg auth

        sendResults cfg result

    case retCode of
        Left err -> putStrLn ("ERROR: " <> show err) >> exitWith (ExitFailure 1)
        Right _  -> putStrLn "ok"

