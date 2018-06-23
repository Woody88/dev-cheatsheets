module Router where
  
import Prelude

import Data.Maybe (Maybe(..))
-- import Effect.Aff (Aff)
import Halogen as H
-- import Halogen.Aff as HA
import Halogen.HTML as HH
-- import Halogen.HTML.Properties as HP
-- import Routing (match)
import Routing.Match (Match, lit)


data Query a 
    = Goto Routes a

data Routes 
    = Home

type State =
  { currentPage :: String
  }

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = home
    where home = Home <$ lit ""


router :: forall m. H.Component HH.HTML Query Unit Void m
router = 
    H.component
        { initialState: const init
        , render
        , eval
        , receiver: const Nothing
        }
  where
    render :: State -> H.ComponentHTML Query
    render st =
        HH.div_ 
            [ HH.h1_ [ HH.text (st.currentPage) ] 
            , viewPage st.currentPage
            ]

    viewPage :: String -> H.ComponentHTML Query
    viewPage path = case path of
        "Home" -> HH.div_ []
        _      -> HH.p_ [] 
    
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Goto Home next) = do
      void $  H.modify (\s -> s { currentPage = "Home" })
      pure next
