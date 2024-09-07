{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Map.Strict            as Map
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text, pack, unpack)
import           Network.Wai.Handler.Warp   (run)
import           Servant.API                (Capture, Get, JSON, OctetStream,
                                             Post, type (:<|>) (..), type (:>))
import           Servant.Multipart          (FileData (fdPayload), Mem,
                                             MultipartData (files),
                                             MultipartForm)
import           Servant.Server             (Application, Handler, Server,
                                             ServerError (..), err400, err404,
                                             errBody, serve)
import           System.Random              (randomRIO)

type API = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Text
        :<|> "mapshot" :> Capture "id" Text :> Get '[OctetStream] BS.ByteString

type Storage = Map.Map String BSC.ByteString

server :: Storage -> Server API
server storage = uploadMapshot storage :<|> retrieveMapshot storage

uploadMapshot :: Storage -> MultipartData Mem -> Handler Text
uploadMapshot storage multipartData = do
    let fs = files multipartData
    case fs of
        [file] -> do
            mapshotId <- liftIO generateId
            let newStorage = Map.insert mapshotId (fdPayload file) storage

            return (pack mapshotId)
        _ ->
            throwError
                err400 { errBody = "Exactly one file must be uploaded" }

retrieveMapshot :: Storage -> Text -> Handler BS.ByteString
retrieveMapshot storage mapshotId = case Map.lookup (unpack mapshotId) storage of
    Just mapshot -> return mapshot
    Nothing      -> throwError err404{errBody = "Mapshot not found"}

generateId :: IO String
generateId = do
    num <- randomRIO (1000000, 9999999 :: Int)
    return $ show num

api :: Proxy API
api = Proxy

app :: Storage -> Application
app storage = serve api (server storage)

main :: IO ()
main = do
    let storage = Map.empty
    putStrLn "Starting server on port 8080"
    run 8080 $ app storage
