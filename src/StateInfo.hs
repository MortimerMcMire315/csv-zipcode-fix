{-# LANGUAGE OverloadedStrings #-}

module StateInfo ( getStateInfo ) where

import Data.Aeson
import Data.Aeson.Types
import Control.Lens               ( (^.)          )
import Network.Wreq               ( get
                                  , responseBody
                                  , responseStatus
                                  , statusCode    )
import Network.HTTP.Client        ( Response,
                                    HttpException )
import Data.ByteString.Lazy.Char8 ( ByteString    )
import Control.Exception          ( try           )

newtype StateAbbrev = StateAbbrev { abbrev :: String }

instance FromJSON StateAbbrev where
    parseJSON = withObject "StateAbbrev" $ \v -> StateAbbrev <$> (v .: "state abbreviation")

unwrapState :: Value -> Parser [StateAbbrev]
unwrapState = withObject "states" $ \o -> o .: "places"

getStateAbbrev :: ByteString -> Either String [StateAbbrev]
getStateAbbrev json = parseEither unwrapState =<< eitherDecode json


reqURL = "http://api.zippopotam.us/us/"

getStateInfo :: String -> IO String
getStateInfo zipCode = do
    eitherRes <- try (get $ reqURL ++ zipCode) :: IO (Either HttpException (Response ByteString))
    case eitherRes of
        Right res -> case getStateAbbrev (res ^. responseBody) of
                        Left err -> error err
                        Right stateLs -> return . abbrev $ head stateLs
        Left e -> do
            print $ "====> ERROR: Zip code not found: " ++ zipCode ++ ": Setting state to empty string."
            return ""
