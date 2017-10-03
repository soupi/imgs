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
import Web.Spock hiding (body, head)
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
        [(p, [])] -> app p

        _ -> do
          hPutStrLn stderr "Could not parse port"
          exitFailure

    [] -> app 8080

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


app :: Int -> IO ()
app port = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock port (spock spockCfg router)

router :: SpockM () () () ()
router = do
  -- internal assets
  for_ assetsDir $ \(path_, file_) -> do
    get (static $ "assets/" <> path_) $
      serveBytes path_ file_

  -- path for img sources
  get ("__imgs_imgs" <//> wildcard) $
    serveImg . T.unpack

  -- everything else
  get (wildcard) $
    servePage . T.unpack

imgsPath :: FilePath
imgsPath = "/__imgs_imgs/"

-------------
-- Servers --
-------------

serveBytes :: MonadIO m => FilePath -> ByteString -> ActionT m a
serveBytes path file' = do
  let mime = Mime.defaultMimeLookup (T.pack path)
  setHeader "content-type" $ T.decodeUtf8 mime
  bytes file'

serveImg :: MonadIO m => FilePath -> ActionT m b
serveImg path = do
  isFile <- liftIO (Dir.doesFileExist path)
  if isFile
    then
      serveFile path
    else do
      setStatus notFound404
      text $ T.pack $ "Image '" <> path <> "' not found."

serveFile :: MonadIO m => FilePath -> ActionT m a
serveFile path = do
  let mime = Mime.defaultMimeLookup (T.pack path)
  file (T.decodeUtf8 mime) path

servePage :: MonadIO m => FilePath -> ActionT m b
servePage path = do
  let currPath = "./" <> path
  (isDir, isFile) <- liftIO $
    (,) <$> Dir.doesDirectoryExist currPath <*> Dir.doesFileExist currPath

  if
    | isDir -> do
        (dirs, imgs) <- liftIO $ getDirAndImages currPath
        let
          prefix
            | currPath == "./" = ""
            | otherwise = "/" <> currPath <> "/"

        lucid $ dirPageTemplate currPath prefix dirs imgs

    | isFile -> do
        (_, imgs) <- liftIO $ getDirAndImages (File.takeDirectory currPath)
        let (prev, next) = getPrevAndNext imgs path
        lucid $ filePageTemplate path prev next

    | otherwise -> do
        setStatus notFound404
        text $ T.pack $ "Directory or file '" <> currPath <> "' not found."


-------

getDirAndImages :: FilePath -> IO ([FilePath], [FilePath])
getDirAndImages currDir = do
  currDirContent <- Dir.listDirectory currDir
  fmap (sort *** sort) $
    (\f -> foldM f ([],[]) currDirContent) $ \(dirs,imgs) file' -> do
      isDir' <- Dir.doesDirectoryExist $ currDir <> "/" <> file'
      if
        | isDir' ->
          pure (file' : dirs, imgs)

        | drop 1 (File.takeExtension file') `elem` (words "png jpg jpeg gif bmp svg")  ->
          pure (dirs, file' : imgs)

        | otherwise -> do
          pure (dirs, imgs)


getPrevAndNext :: [FilePath] -> FilePath -> (FilePath, FilePath)
getPrevAndNext imgs path =
  let
    addDir x = File.takeDirectory path <> "/" <> x
    mIndex = findIndex (File.takeFileName path ==) imgs
  in (addDir *** addDir) $ case mIndex of
    Nothing -> (path, path) -- this shouldn't happen if isDir is true but we'll handle that
    Just 0 ->
      ( last imgs
      , last $ take 2 imgs
      )
    Just i | i + 1 == length imgs ->
      ( head $ drop (i - 1) imgs
      , head imgs
      )
    Just i ->
      ( head $ drop (i - 1) imgs
      , last $ take 3 $ drop (i - 1) imgs
      )

----------
-- Html --
----------

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


dirPageTemplate :: String -> String -> [String] -> [String] -> Html
dirPageTemplate currDir prefix dirs imgs = template currDir $ do
  H.h2_ $ H.toHtml currDir
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

filePageTemplate :: FilePath -> FilePath -> FilePath -> Html
filePageTemplate path prev next =
  template path $ do
    H.h2_ $ H.toHtml path
    H.div_ [ H.class_ "imgNav" ] $ do
      H.a_ [ H.href_ $ T.pack $ "/" <> prev ] $ do
        H.toHtml $ T.pack "<< prev "
      H.a_ [ H.href_ $ T.pack $ "/" <> next ] $ do
        H.toHtml $ T.pack " next >>"
    H.a_ [ H.href_ $ T.pack $ "/" <> File.takeDirectory path ] $ do
      H.img_ [ H.src_ $ T.pack $ imgsPath <> path ]

