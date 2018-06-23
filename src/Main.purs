module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Router as R

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI R.router unit body
