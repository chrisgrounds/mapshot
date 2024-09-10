{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Base64.Types           (Alphabet (..), Base64 (..),
                                              extractBase64)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Lazy.Char8  as BSC
import qualified Data.Map.Strict             as Map
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text, pack, take, unpack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Text.Encoding.Base64   (decodeBase64, encodeBase64)
import           Data.Text.IO                (writeFile)
import           Network.URI.Encode          (encodeText)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Prelude                     hiding (take, writeFile)
import           Servant.API                 (Capture, Get, JSON, OctetStream,
                                              Post, type (:<|>) (..), type (:>))
import           Servant.Multipart           (FileData (fdPayload), Mem,
                                              MultipartData (files),
                                              MultipartForm)
import           Servant.Server              (Application, Handler, Server,
                                              ServerError (..), err400, err404,
                                              errBody, serve)
import           System.Directory            (doesFileExist)
import           System.Random               (randomRIO)

type API = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Text
        :<|> "mapshot" :> Capture "id" Text :> Get '[OctetStream] BS.ByteString

type Base64Text = Base64 'StdPadded Text

server :: Server API
server = uploadMapshot :<|> retrieveMapshot

fileToBase64 :: [FileData Mem] -> Base64Text
fileToBase64 = encodeBase64 . decodeUtf8 . BS.toStrict . mconcat . fmap fdPayload

fileToString :: [FileData Mem] -> Text
fileToString = decodeUtf8 . BS.toStrict . mconcat . fmap fdPayload

textToByteString :: Text -> BS.ByteString
textToByteString = BS.fromStrict . encodeUtf8

uploadMapshot :: MultipartData Mem -> Handler Text
uploadMapshot multipartData = do
    liftIO $ putStrLn "Starting upload process"

    let fs = files multipartData
    let baseString = extractBase64 $ fileToBase64 fs
    let mapshotId = encodeText $ take 20 baseString

    liftIO $ writeFile (unpack mapshotId) baseString
    liftIO $ putStrLn "Upload process finished"

    return mapshotId

retrieveMapshot :: Text -> Handler BS.ByteString
retrieveMapshot mapshotId = do
    exists <- liftIO (doesFileExist $ unpack mapshotId)
    if exists
        then do
            mapshot <- liftIO (readFile $ unpack mapshotId)
            liftIO $ putStrLn $ "Retrieving mapshot: " ++ show mapshot
            return $ textToByteString $ pack mapshot
        else
            throwError err404 {errBody = "Mapshot not found"}

api :: Proxy API
api = Proxy

app :: Application
app = simpleCors $ serve api server

main :: IO ()
main = do
    putStrLn "Starting server on port 8080"
    run 8080 app
