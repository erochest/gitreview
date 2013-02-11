{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Github.Review.Format
    ( formatCommit
    , formatCommitText
    , formatCommitHtml
    , commentEmail
    ) where


import qualified Data.Text as T
import           Github.Data
import           Network.Mail.Mime
import           Text.Blaze.Html5 hiding (map, span, summary)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (id, span, summary)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text (renderHtml)

formatCommit :: Commit -> (T.Text, Html)
formatCommit c = (formatCommitText c, formatCommitHtml c)

formatCommitText :: Commit -> T.Text
formatCommitText = undefined

formatCommitHtml :: Commit -> Html
formatCommitHtml = undefined

commentEmail :: Commit -> Mail
commentEmail = undefined
