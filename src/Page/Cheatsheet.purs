module Page.Cheatsheet where

import Prelude

import App.Monad (App)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = 
    { name :: String }

type Input = State 

data Query a 
    = HandleInput Input a

type Slot = H.Slot Query Void

init :: State
init = { name: "" }

component :: H.Component HH.HTML Query Input Void App
component = 
    H.component
        { initialState: (\i -> i)
        , render
        , eval
        , receiver: HE.input HandleInput
        , initializer: Nothing 
        , finalizer: Nothing
        }
  where
    render :: State -> H.ComponentHTML Query () App
    render st =
        HH.h1_ [ HH.text st.name ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (HandleInput state next) = do
      oldState <- H.get
      when (oldState /= state) $ H.put state
      pure next
