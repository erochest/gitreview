{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE EmptyDataDecls    #-}


module Github.Review.Format
    ( formatCommit
    , formatCommitText
    , formatCommitHtml
    , commentEmail
    , formatError
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Text.Lazy (fromStrict, toStrict)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
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
    <> "by   " <> fullEmail (fromString gitUserName) (fromString gitUserEmail) <> "\n"
    <> "on   " <> fromString (show (fromGithubDate gitUserDate)) <> nl
    where GitUser{..} = gitCommitCommitter commitGitCommit
          puri        = fmap (show . toGithubUri) . parseURI
          maybeUri l  = maybe mempty (\s -> l <> ": " <> fromString s <> "\n") . puri
          nl          = "\n"
          nlnl        = "\n\n"

formatCommitHtml :: Repo -> Commit -> Html
formatCommitHtml Repo{..} Commit{..} = docTypeHtml $ do
    H.head $ H.title "Commit to Review"
    body $ do
        h1 "Commit to Review"

        h2 repoSpec
        p $ do
            b $ a ! href (toValue repoHtmlUrl) $ repoSpec
            br
            toHtml $ fromMaybe "" repoDescription
        dl $ do
            dpair "URL" $ urlA repoHtmlUrl
            dpair "GIT" $ toHtml repoGitUrl

        h2 $ toHtml $ "Commit " <> shortSha'
        p $ do
            case fmap (show . toGithubUri) $ parseURI commitUrl of
                Nothing  -> "Invalid commit URL."
                Just url -> b $ a ! href (toValue url) $ sha
            br
            toHtml . gitCommitMessage $ commitGitCommit
        dl $ do
            dpair "SHA" sha
            dpair "by" $ toHtml $ fullEmail gitUserName gitUserEmail
            dpair "on" $ toHtml $ show (fromGithubDate gitUserDate)

    where GitUser{..}    = gitCommitCommitter commitGitCommit
          urlA url       = a ! href (toValue url) $ toHtml url
          dpair term def = dt term >> dd def
          repoSpec       = toHtml $ githubOwnerLogin repoOwner <> "/" <> repoName
          sha            = toHtml commitSha
          shortSha'      = take 8 commitSha

fullEmail :: (Monoid m, IsString m) => m -> m -> m
fullEmail name email = name <> " <" <> email <> ">"

commentEmail :: Address -> Address -> T.Text -> Repo -> Commit -> IO Mail
commentEmail to from subject r c =
        simpleMail to from subject (fromStrict asText) (renderHtml asHtml) []
        where (asText, asHtml) = formatCommit r c

formatError :: Error -> [TaskName] -> (T.Text, Html)
formatError err tasks =
        ( render $  "ERROR: At task " <> fromText task <> "\n"
                 <> fromString errStr <> "\n\n"
                 <> taskListText tasks
        , docTypeHtml $ do
            H.head $ H.title "ERROR Retreiving Commits to Review"
            body $ do
                h1 "ERROR"
                p $ do
                    toHtml ("At task " :: T.Text)
                    em $ toHtml task
                pre $ code $ toHtml (show err)
                taskListHtml tasks
        )
        where errStr = show err
              task   = last tasks
              render = toStrict . toLazyText

taskListText :: [TaskName] -> Builder
taskListText = foldr f empty . zip [1..] . map fromText
    where f (i, task) buffer = decimal i <> (". " <> (task <> ("\n" <> buffer)))
          empty = fromText ""

taskListHtml :: [TaskName] -> Html
taskListHtml tasks = ol $ forM_ tasks $ li . toHtml

