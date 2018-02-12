{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Data.JCDecaux.Types where
import Data.Semigroup
import Data.Text (Text(..), pack, unpack)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Char
import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free
import GHC.Generics
import Network.Wreq as Wreq

toSnakeCase [] = []
toSnakeCase (x:xs) = case x of
    '_' -> snakeCaseInner xs
    _ -> snakeCaseInner (x:xs)
  where
    snakeCaseInner (x:y:xs) = x : if
        isLower x && isUpper y
      then '_' : toLower y : snakeCaseInner xs
      else snakeCaseInner (y:xs)
    snakeCaseInner x = x

aesonOptions = defaultOptions{fieldLabelModifier=toSnakeCase}

data Contract = Contract
  { _name :: Text
  , _cities :: [Text]
  , _commercialName :: Text
  , _countryCode :: Text
  } deriving (Eq, Show, Generic)
makeFieldsNoPrefix ''Contract
instance ToJSON Contract where
  toJSON = genericToJSON aesonOptions

instance FromJSON Contract where
  parseJSON = genericParseJSON aesonOptions

data WGS84Position = Pos
  { _latitude :: Double
  , _longitude :: Double
  } deriving (Eq, Show)
makeLenses ''WGS84Position
instance ToJSON WGS84Position where
  toJSON (Pos lat lng) = object ["lat" .= lat, "lng" .= lng]
instance FromJSON WGS84Position where
  parseJSON = withObject "WGS84Position" $ \obj -> Pos
    <$> obj .: "lat"
    <*> obj .: "lng"

-- Whether a station is CLOSED or OPEN
data StationStatus = SOpen | SClosed deriving (Eq, Show)
instance ToJSON StationStatus where
  toJSON SOpen = String "OPEN"
  toJSON SClosed = String "CLOSED"
instance FromJSON StationStatus where
  parseJSON = withText "StationStatus" $ \txt ->
    case txt of
      "OPEN" -> pure SOpen
      "CLOSED" -> pure SClosed
      v -> typeMismatch "StationStatus" (String v)

data Station = Station
  { _number :: Int
  , _contractName :: Text
  , _name :: Text -- name of the station
  , _address :: Text -- address of the station. As it is raw data, sometimes it will be more of a comment than an address
  , _position :: WGS84Position
  , _banking :: Bool -- indicates whether this station has a payment terminal
  , _bonus :: Bool -- indicates whether this is a bonus station
  , _status :: StationStatus --indicates
  , _bikeStands :: Int
  , _availableBikeStands :: Int
  , _availableBikes :: Int
  , _lastUpdate :: Integer -- last update in milliseconds since last unix epoch
  } deriving (Eq, Show, Generic)
makeFieldsNoPrefix ''Station

instance ToJSON Station where
  toJSON = genericToJSON aesonOptions

instance FromJSON Station where
  parseJSON = genericParseJSON aesonOptions

data BikeAST a where
  GetContracts :: ([Contract] -> a) -> BikeAST a
  GetStations :: Maybe Text -> ([Station] -> a) -> BikeAST a
  GetStation :: Int -> Text -> (Station -> a) -> BikeAST a
  deriving Functor

type BikeAPI m a = FreeT BikeAST m a

getContracts :: Monad m => BikeAPI m [Contract]
getContracts = liftF $ GetContracts id

getAllStations :: Monad m => BikeAPI m [Station]
getAllStations = liftF $ GetStations Nothing id

getStationsForContract :: Monad m => Text -> BikeAPI m [Station]
getStationsForContract contractName =
  liftF $ GetStations (Just contractName) id

getStation :: Monad m => Int -> Text -> BikeAPI m Station
getStation staionNumber contractName =
  liftF $ GetStation staionNumber contractName id

runBikeAPI :: (Monad m, MonadIO m) => Text -> BikeAPI m a -> ExceptT String m a
runBikeAPI apiKey ft = do
  freeVal <- lift $ runFreeT ft
  case freeVal of
    Pure a -> pure a
    Free (GetContracts f) -> doReq f "contracts" options
    Free (GetStations mContract f) -> let run = doReq f "stations" in
      case mContract of
        Just cName -> run $ options & param "contract" .~ [cName]
        Nothing -> run options
    Free (GetStation sNum cName f) ->
      doReq f ("st  ation/" <> show sNum) $ options & param "contract" .~ [cName]
    where
      apiBase = "https://api.jcdecaux.com/vls/v1/"
      options = defaults & param "apiKey" .~ [apiKey]
      doReq :: (FromJSON b, Monad m, MonadIO m) => (b -> BikeAPI m a) -> String -> Wreq.Options -> ExceptT String m a
      doReq f urlSuffix opt = do
        res <- liftIO . getWith opt $ apiBase <> urlSuffix
        let eVal = eitherDecode $ res ^. responseBody
        case eVal of
          Left err -> throwE err
          Right val -> runBikeAPI apiKey $ f val
