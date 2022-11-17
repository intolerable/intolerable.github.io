module Pages where

import Control.Monad.IO.Class
import Lucid
import Servant
import Servant.Links
import Network.Wai.Handler.Warp
import Lucid.Servant
import Data.Text (Text)

import Pages.CSS
import Pages.Layout

generatePages :: IO ()
generatePages = do
  generate (Proxy :: Proxy API) EmptyContext static >>= print 
  writeStaticDirectory (Proxy :: Proxy API) EmptyContext static "docs"
  putStrLn "running on port 8080"
  run 8080 $ serve (Proxy :: Proxy API) server 

static :: MonadIO m => StaticT API m
static = indexHandler :<|> styleHandler

server :: MonadIO m => ServerT API m
server = static

styleLink :: Link
_ :<|> styleLink = allLinks (Proxy :: Proxy API) 

indexHandler :: Monad m => m (Html ())
indexHandler = pure do
  doctype_ 
  html_ do
    head_ do
      title_ "intolerable.github.io"
      link_ [rel_ "stylesheet", linkAbsHref_ styleLink]
    body_ do
      main_ do
        h1_ [id_ "title"] "intolerable"
        h2_ [id_ "name"] "Fraser Murray"
        h3_ [id_ "email"] "fraser (dot) m (dot) murray (at) gmail (dot) com"
        div_ [id_ "links"] do
          div_ [id_ "github"] do
            linkTo "GitHub" "https://github.com/intolerable"
          div_ [id_ "attunement"] do
            linkTo "Attunement" "https://attunement.io"
            p_ do
              "Attunement is a card database and advanced search engine I built for the Legends of Runeterra collectible card game. It\8217s written in Haskell using "
              a_ [href_ "https://github.com/haskell-servant/servant"] "Servant"
              " and "
              a_ [href_ "https://hackage.haskell.org/package/warp"] "Warp"
              ". It\8217s backed by a PostgreSQL database and uses DigitalOcean\8217s S3-like "
              a_ [href_ "https://www.digitalocean.com/products/spaces"] "Spaces"
              " for image storage. The front-end is almost entirely server-rendered HTML, with some SPA-like features in the admin console using "
              a_ [href_ "https://htmx.org"] "HTMX"
              "."

linkTo :: Text -> Text -> Html ()
linkTo l u =
  a_ [class_ "link", href_ u] do
    toHtml l
    span_ [class_ "url"] do
      "("
      toHtml u
      ")"