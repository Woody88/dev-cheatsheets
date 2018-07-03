module Page.Home where
  
import Prelude

import App.Monad (App)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH


type State = Int

data Query a 
    = Query Unit a

type Slot = H.Slot Query Void

component :: H.Component HH.HTML Query Unit Void App
component = 
    H.component
        { initialState: const 0
        , render
        , eval
        , receiver: const Nothing 
        , initializer: Nothing
        , finalizer: Nothing
        }
  where
    render :: State -> H.ComponentHTML Query () App
    render st =
        HH.h1_ [ HH.text "Home" ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (Query _ next) = do
      pure next
