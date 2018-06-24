module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface)
import Router as R


main :: Effect Unit
main = HA.runHalogenAff $ do
  nav  <- liftEffect $ makeInterface
  body <- HA.awaitBody
  location <- liftEffect $ nav.locationState
  void $ liftEffect $ log location.path        --- Debug purposes
  runUI R.router location.path body