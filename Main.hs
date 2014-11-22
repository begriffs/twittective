{-# LANGUAGE OverloadedStrings #-}

-- import Text.HTML.TagSoup
-- import Text.HTML.TagSoup.Tree

import GHC.Int
import Data.Maybe
import Control.Applicative ( (<$>) )
import Network.HTTP.Conduit
import Data.String.Conversions (cs)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy as BL
import Data.Text

type TweetId = Int64

timelineURI :: String -> Maybe TweetId -> String
timelineURI tweep maxTweetId =
  "https://twitter.com/i/profiles/show/" ++ tweep ++ "/timeline?include_entities=1" ++
    fromMaybe "" (("&max_id=" ++) . show <$> maxTweetId)

main :: IO ()
main = do
  body <- simpleHttp (timelineURI "drboolean" Nothing)
  let html = fromMaybe "" $ htmlPayload body

  Prelude.putStr $ cs html


htmlPayload :: BL.ByteString -> Maybe Text
htmlPayload v = do
  obj <- decode v
  flip parseMaybe obj $ \x -> x .: "items_html"
