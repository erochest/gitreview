{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Github.Review.Format
    ( formatCommit
    , formatCommitText
    , formatCommitHtml
    , commentEmail
    ) where


import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder
import           Github.Data
import           Github.Review
import           Network.Mail.Mime
import           Network.URI
import           Text.Blaze.Html5 hiding (map, span, summary)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (id, span, summary)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text (renderHtml)


formatCommit :: Repo -> Commit -> (T.Text, Html)
formatCommit r c = (formatCommitText r c, formatCommitHtml r c)

formatCommitText :: Repo -> Commit -> T.Text
formatCommitText Repo{..} Commit{..} = toStrict . toLazyText $
       "Commit to review:\n\n"
    <> fromString (githubOwnerLogin repoOwner) <> "/" <> fromString repoName <> nl
    <> maybe mempty fromString repoDescription <> nlnl
    <> "URI: " <> fromString repoHtmlUrl <> nl
    <> "GIT: " <> fromString repoGitUrl <> nlnl
    <> "Commit:\n"
    <> fromString (gitCommitMessage commitGitCommit) <> nlnl
    <> maybeUri "URL" commitUrl
    <> "SHA: " <> fromString commitSha <> nl
    <> "by   " <> fromString gitUserName <> " <" <> fromString gitUserEmail <> ">\n"
    <> "on   " <> fromString (show (fromGithubDate gitUserDate)) <> nl
    where GitUser{..} = gitCommitCommitter commitGitCommit
          puri        = fmap (show . toGithubUri) . parseURI
          maybeUri l  = maybe mempty (\s -> l <> ": " <> fromString s <> "\n") . puri
          nl          = "\n"
          nlnl        = "\n\n"

formatCommitHtml :: Repo -> Commit -> Html
formatCommitHtml = undefined

commentEmail :: Repo -> Commit -> Mail
commentEmail = undefined
