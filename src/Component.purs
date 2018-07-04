module Component where
  
import Prelude

import Component.NavBar as NavBar
import Data.Symbol (SProxy(..))

type CompnentSlots r =
    (navbar :: NavBar.Slot Unit 
    | r
    )

_navbar = SProxy :: SProxy "navbar"

navbarComponent = NavBar.component
