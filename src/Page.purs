module Page where

import Prelude 

import Data.Symbol (SProxy(..))
import Page.Home as Home
import Page.NotFound as NotFound

type PageSlots =
  ( homePage       :: Home.Slot Unit
  , notfoundPage   :: NotFound.Slot Unit
  )

_homePage     = SProxy :: SProxy "homePage"
_notfoundPage = SProxy :: SProxy "notfoundPage"

homeComponent     = Home.component
notfoundComponent = NotFound.component