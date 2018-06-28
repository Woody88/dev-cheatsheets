module Page.Home where
  
import Prelude

import Data.Maybe (Maybe(..))
import App.Monad (App)
import Halogen as H
import Halogen.HTML as HH


type State = Int

data Query a 
    = Query Unit a

component :: H.Component HH.HTML Query Unit Void App
component = 
    H.component
        { initialState: const 0
        , render
        , eval
        , receiver: const Nothing 
        }
  where
    render :: State -> H.ComponentHTML Query
    render st =
        HH.h1_ [ HH.text "Home" ]

    eval :: Query ~> H.ComponentDSL State Query Void App
    eval (Query _ next) = do
      pure next
