module Page where

import Prelude

import Data.Symbol (SProxy(..))
import Page.Cheatsheet as Cheatsheet
import Page.Home as Home
import Page.NotFound as NotFound
import Router (Route(..))

type PageSlots r =
  ( homePage       :: Home.Slot Unit
  , notfoundPage   :: NotFound.Slot Unit
  , cheatsheetPage :: Cheatsheet.Slot Unit
  | r
  )

_homePage       = SProxy :: SProxy "homePage"
_notfoundPage   = SProxy :: SProxy "notfoundPage"
_cheatsheetPage = SProxy :: SProxy "cheatsheetPage"

homeComponent       = Home.component
notfoundComponent   = NotFound.component
cheatsheetComponent = Cheatsheet.component