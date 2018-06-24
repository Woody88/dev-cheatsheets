module Router where
  
import Prelude

import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Routing (match)
import Routing.Match (Match, lit, end)
import Web.HTML.HTMLDocument.ReadyState (parse)


data Query a 
    = Goto Route a
    | Location String a

data Route
    = Home
    | NotFound

instance showRoutes :: Show Route where
  show (Home)     = "Home"
  show (NotFound) = "NotFound"

type State =
  { currentPage :: String
  }

parseRoute :: String -> Route
parseRoute = parseRouteEither <<< match routing
    where parseRouteEither = either (const NotFound) identity

routing :: Match Route
routing = oneOf
    [ home
    , pure NotFound
    ]
    where home = Home <$ lit "" <* end
         

router :: H.Component HH.HTML Query String Void Aff
router = 
    H.component
        { initialState: (\path -> { currentPage: show $ parseRoute path })
        , render
        , eval
        , receiver: HE.input Location  -- Not sure if this is ever called, need some clarification
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
        "Home" -> HH.div_ [ HH.button [ HE.onClick (HE.input_ $ Goto NotFound) ] [ HH.text "Not Found" ] ]
        _      -> HH.p_ [] 
    
    eval :: Query ~> H.ComponentDSL State Query Void Aff
    eval (Goto NotFound next) = do
      void $  H.modify (\s -> s { currentPage = "NotFound" })
      pure next
    eval (Goto Home next) = do
      void $  H.modify (\s -> s { currentPage = "Home" })
      pure next
    eval (Location path next) = do
       HA.liftEffect $ log $ "Path: " <> path
       eval $ Goto (parseRoute path) next
    
