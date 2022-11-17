module Pages.Layout where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Kind
import Data.Map.Strict (Map)
import Lucid
import Servant
import Servant.API.ContentTypes
import Servant.HTML.Lucid
import System.Directory
import System.FilePath
import GHC.TypeLits
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map as Map

import Pages.CSS

type API = 
  "index.html" :> Get '[HTML] (Html ()) :<|>
  "style.css" :> Get '[CSS] CSS 

newtype FilePathSpec = FilePathSpec [String]
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

prependSection :: String -> FilePathSpec -> FilePathSpec
prependSection s (FilePathSpec fps) = FilePathSpec (s : fps)

filePathSpecToFilePath :: FilePath -> FilePathSpec -> FilePath
filePathSpecToFilePath base (FilePathSpec spec) =
  foldl (</>) base spec

class HasStatic a ctx where
  type StaticT a (m :: Type -> Type) :: Type

  generate :: Proxy a -> Context ctx -> StaticT a IO -> IO (Map FilePathSpec ByteString)

  hoistStaticWithContext :: Proxy a -> Proxy ctx -> (forall x . m x -> n x) -> StaticT a m -> StaticT a n

instance FirstMimeRender cts a => HasStatic (Verb meth stat cts a) ctx where
  type StaticT (Verb meth stat cts a) m = m a

  generate _ ctx act = do
    res <- act
    pure $ Map.singleton mempty (firstMimeRender (Proxy :: Proxy cts) res)

  hoistStaticWithContext _ _ nt sub = nt sub

instance (KnownSymbol sym, HasStatic a ctx) => HasStatic (sym :> a) ctx where
  type StaticT (sym :> a) m = StaticT a m

  generate (Proxy :: Proxy (sym :> a)) ctx sub = do
    let symV = symbolVal (Proxy :: Proxy sym)
    Map.mapKeys (prependSection symV) <$> generate (Proxy :: Proxy a) ctx sub

  hoistStaticWithContext (Proxy :: Proxy (sym :> a)) ctx l =
    hoistStaticWithContext (Proxy :: Proxy a) ctx l

instance (HasStatic a ctx, HasStatic b ctx) => HasStatic (a :<|> b) ctx where
  type StaticT (a :<|> b) m = StaticT a m :<|> StaticT b m

  generate (Proxy :: Proxy (a :<|> b)) ctx (l :<|> r) = do
    mappend <$> generate (Proxy :: Proxy a) ctx l
            <*> generate (Proxy :: Proxy b) ctx r

  hoistStaticWithContext (Proxy :: Proxy (a :<|> b)) ctx nt (l :<|> r) = 
    hoistStaticWithContext (Proxy :: Proxy a) ctx nt l :<|>
    hoistStaticWithContext (Proxy :: Proxy b) ctx nt r

writeStaticDirectory :: HasStatic a ctx
                     => Proxy a -> Context ctx -> StaticT a IO -> FilePath -> IO ()
writeStaticDirectory p ctx act fp = do
  res <- generate p ctx act
  void $ Map.traverseWithKey go res
  where go :: FilePathSpec -> ByteString -> IO ()
        go fps bs = do
          let targetFilePath = filePathSpecToFilePath fp fps
          putStrLn $ "creating " <> targetFilePath
          createDirectoryIfMissing True $ takeDirectory targetFilePath
          ByteString.writeFile targetFilePath bs

class FirstMimeRender (cts :: [Type]) (a :: Type) where
  firstMimeRender :: Proxy cts -> a -> ByteString 

instance MimeRender ct a => FirstMimeRender (ct ': cts) a where
  firstMimeRender _ = mimeRender (Proxy :: Proxy ct) 