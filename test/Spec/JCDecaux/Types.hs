module Spec.JCDecaux.Types where
import Data.JCDecaux.Types
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as R
import Data.Aeson

contract :: MonadGen m => m Contract
contract =
  let mkText = text (linear 1 10) ascii in
    Contract
      <$> mkText
      <*> list (linear 1 2) mkText
      <*> mkText
      <*> text (R.constant 1 3) upper


prop_contractRoundTrip :: Property
prop_contractRoundTrip = property $ do
  c <- forAll contract
  let trip = fromJSON . toJSON $ c
  trip === Success c

pos :: MonadGen m => m WGS84Position
pos = Pos <$> c <*> c
  where
    c = (double (R.constant (-180.0) (180.0)))

station :: MonadGen m => m Station
station = do
  stands <- int $ linear 1 10
  avail <- int $ R.constant 0 stands
  let
    basicText = text (linear 1 10) ascii
    result = Station
      <$> int (linear 0 100)
      <*> basicText
      <*> basicText
      <*> basicText
      <*> pos
      <*> bool_
      <*> bool_
      <*> element [SClosed, SOpen]
      <*> pure stands
      <*> pure avail
      <*> pure (stands - avail)
      <*> integral (R.constant 1200000000000 1800000000000)
  result

prop_stationRoundTrip :: Property
prop_stationRoundTrip = property $ do
  s <- forAll station
  let trip = fromJSON . toJSON $ s
  trip === Success s