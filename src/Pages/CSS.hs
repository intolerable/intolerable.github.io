module Pages.CSS where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (ByteString)
import Servant
import Network.HTTP.Media ((//))
import Text.Sass qualified as Sass
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Paths_intolerable_github_io

newtype CSS = CSS ByteString
  deriving (Show, Read, Eq, Ord)

instance Accept CSS where
  contentType Proxy = "text" // "css"

instance MimeRender CSS CSS where
  mimeRender Proxy (CSS s) = s

styleHandler :: MonadIO m => m CSS
styleHandler = liftIO do
  fp <- getDataFileName "resources/styles/main.sass"
  let opts = Sass.defaultSassOptions {
        Sass.sassOutputStyle = Sass.SassStyleCompressed
      }
  Sass.compileFile fp opts >>= \case
    Left err -> Sass.errorMessage err >>= error
    Right res ->
      pure $ CSS $ ByteString.fromStrict $ Sass.resultString res
