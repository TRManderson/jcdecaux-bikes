{-# LANGUAGE OverloadedStrings #-}
import Hedgehog
import Spec.JCDecaux.Types

tests :: IO Bool
tests =
  checkParallel . Group "JCDecaux.Types" $
    [ ("prop_contractRoundTrip", prop_contractRoundTrip)
    , ("prop_stationRoundTrip", prop_stationRoundTrip)
    ]

main :: IO ()
main = tests >> pure ()
