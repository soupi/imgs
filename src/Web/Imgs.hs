{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Web.Imgs where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Mime as Mime (defaultMimeLookup)
import qualified System.Directory as Dir
import qualified System.FilePath  as File
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.List
import Data.Foldable
import Web.Spock hiding (body)
import Web.Spock.Config
import Network.HTTP.Types.Status
import Web.Spock.Lucid
import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Lucid as H

import Data.ByteString (ByteString)
import Data.FileEmbed

assetsDir :: [(FilePath, ByteString)]
assetsDir = $(embedDir "assets")

run :: IO ()
run =
  getArgs >>= \case
    args
      | "--help" `elem` args ->
        putStrLn helpMessage

    [ "--port", port ] -> do
      case reads port of
        [(p, [])] -> do
          spockCfg <- defaultSpockCfg () PCNoDatabase ()
          runSpock p (spock spockCfg app)

        _ -> do
          hPutStrLn stderr "Could not parse port"
          exitFailure

    [] -> do
      spockCfg <- defaultSpockCfg () PCNoDatabase ()
      runSpock 8080 (spock spockCfg app)

    args -> do
      hPutStrLn stderr $ "Invalid arguments: " <> concat args
      exitFailure

helpMessage :: String
helpMessage = unlines
  [ "Imgs - a minimalistic image album server"
  , ""
  , "usage: imgs [ --port PORT, --help ]"
  , ""
  , "  --port      Port to run the server. default is 8080"
  , "  --help      Show this help message"
  ]


imgsPath :: FilePath
imgsPath = "/__imgs_imgs/"

app :: SpockM () () () ()
app = do
  for_ assetsDir $ \(path_, file_) -> do
    get (static $ "assets/" <> path_) $ do
      let mime = Mime.defaultMimeLookup (T.pack path_)
      setHeader "content-type" $ T.decodeUtf8 mime
      bytes file_

  get ("__imgs_imgs" <//> wildcard) $ \(T.unpack -> path) -> do
    isFile <- liftIO $ Dir.doesFileExist path
    if
      | isFile -> do
        let mime = Mime.defaultMimeLookup (T.pack path)
        file (T.decodeUtf8 mime) path

      | otherwise -> do
          setStatus notFound404
          text $ T.pack $ "Image '" <> path <> "' not found."

  get (wildcard) $ \(T.unpack -> path) -> do
    let currDir = "./" <> path
    (isDir, isFile) <- liftIO $
      (,) <$> Dir.doesDirectoryExist currDir <*> Dir.doesFileExist currDir

    if
      | isDir -> do
        currDirContent <- liftIO $ Dir.listDirectory currDir
        (dirs, imgs) <- liftIO $ fmap (sort *** sort) $
          (\f -> foldM f ([],[]) currDirContent) $ \(dirs,imgs) file' -> do
            isDir' <- Dir.doesDirectoryExist $ currDir <> "/" <> file'
            if
              | isDir' ->
                pure (file' : dirs, imgs)

              | drop 1 (File.takeExtension file') `elem` (words "png jpg jpeg gif bmp svg")  ->
                pure (dirs, file' : imgs)

              | otherwise -> do
                pure (dirs, imgs)

        let
          prefix
            | currDir == "./" = ""
            | otherwise = "/" <> currDir <> "/"

{-
        lucid $ template currDir $ do
          H.h1_ $ H.toHtml currDir
          H.ul_ $
            mapM_
              (\l -> H.li_ . H.a_ [ H.href_ $ T.pack (prefix <> l) ] $ H.toHtml l)
              $ ".." : dirs ++ imgs
-}

        lucid $ template currDir $ do
          H.h1_ $ H.toHtml currDir
          H.ul_ [ H.class_ "dirs" ] $ do
            H.li_ [ H.class_ "up" ] $
              H.a_ [ H.href_ $ T.pack (prefix <> "..") ] $ do
                H.img_ [ H.src_ $ T.pack "/assets/images/up.png", H.class_ "thumbnail" ]
                H.p_ $ H.toHtml $ T.pack ".."

            flip mapM_ dirs $ \l ->
              H.li_ . H.a_ [ H.href_ $ T.pack (prefix <> l) ] $ do
                H.img_ [ H.src_ $ T.pack "/assets/images/dir.png", H.class_ "thumbnail" ]
                H.p_ $ H.toHtml $ if (length l > 15) then take 12 l <> "..." else l

            flip mapM_ imgs $ \l ->
              H.li_ . H.a_ [ H.href_ $ T.pack (prefix <> l) ] $ do
                H.img_ [ H.src_ $ T.pack $ imgsPath <> prefix <> l, H.class_ "thumbnail" ]
                H.p_ $ H.toHtml $ if (length l > 15) then take 12 l <> "..." else l


      | isFile -> do
        lucid $ template currDir $ do
          H.h1_ $ H.toHtml currDir
          H.a_ [ H.href_ $ T.pack $ "/" <> File.takeDirectory path ] $ do
            H.img_ [ H.src_ $ T.pack $ imgsPath <> path ]

      | otherwise -> do
          setStatus notFound404
          text $ T.pack $ "Directory or file '" <> currDir <> "' not found."

type Html = H.Html ()

template :: String -> Html -> Html
template title body =
  H.doctypehtml_ $ do
    H.head_ $ do
      H.meta_ [ H.charset_ "utf-8" ]

      H.title_ (H.toHtml $ "Imgs - " <> title)

      H.meta_ [ H.name_ "viewport", H.content_ "width=device-width, initial-scale=1" ]

      H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/assets/css/style.css"  ]

    H.body_ $ do
      H.div_ [ H.class_ "main" ] body
