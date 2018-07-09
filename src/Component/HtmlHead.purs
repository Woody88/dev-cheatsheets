module Component.HtmlHead where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Router (Route(..))

type State = Route

type Input = Unit

type Message = Void

data Query a 
    = ChangeTitle State a

component :: forall m. H.Component HH.HTML Query Input Message m
component = 
    H.component
        { initialState: const Home
        , render
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }
    where 
        render :: forall m. State -> H.ComponentHTML Query () m
        render Home  = HH.title [] [ HH.text "Dev-Cheatsheet" ]
        render state = HH.title [] [ HH.text $ (show state) <> " - Cheatsheet" ] 

        eval :: forall m. Query ~> H.HalogenM State Query () Message m
        eval (ChangeTitle route next) = do
            H.put route
            pure next