module Page.Cheatsheet where


import Node.FS.Sync
import Prelude

import App.Monad (App)
import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (ClassName(..), HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Html.Parser.Halogen as PH

type State = 
    { name      :: String 
    }

type Input = State 

data Query a 
    = HandleInput Input a

type Slot = H.Slot Query Void

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
    render st = do
        HH.div [ HP.class_ (ClassName "uk-text-center") ]
            [ HH.h3 [ HP.class_ (ClassName "uk-heading-primary") ] 
                [ HH.text st.name 
                , HH.span [ HP.class_ (ClassName "uk-text-muted") ] 
                    [ HH.text " Cheatsheet" ]  
                ]
            -- , HH.div_ ( map renderCheatsheet data_)
            ]

    -- renderCheatsheet :: CheatData -> H.ComponentHTML Query () App
    -- renderCheatsheet cheatdata = do
    --     HH.div_ 
    --         [ HH.h4 [ HP.class_ (ClassName "uk-heading-line") ] 
    --             [ HH.span_ [ HH.text cheatdata.title ]
    --             , PH.render $ cheatdata.html
    --             ]
    --         ]

    eval :: Query ~> H.HalogenM State Query () Void App
    eval (HandleInput state next) = do
      oldState <- H.get
      when (oldState /= state) $ H.put state
      pure next

-- This will be replaced by github queries
-- loadFiles :: Array FilePath -> String -> Effect (Array CheatData)
-- loadFiles files dir = sequence $ map loadFile files
--     where cheatdata fname h = pure { title: fname, html: h }
--           loadFile filename = readTextFile UTF8 ("./Cheatsheets/" <> dir <> filename) >>= cheatdata filename

-- loadCheatsheetFiles :: State -> Effect (Array String)
-- loadCheatsheetFiles state = readdir dirPath
--     where dirPath = "./Cheatsheets/" <> state.name

foreign import data Markdown :: Type
foreign import markdown :: Markdown
foreign import markdownRender :: String -> Markdown -> Effect String
