module Github where 


import Github.Types
import Prelude

import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Fiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.HTML (a)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes

repo = "https://api.github.com/repos/Woody88/dev-cheatsheets/contents/"
    
main = launchAff_ $ do
  res <- AX.affjax AXRes.json (AX.defaultRequest { url = repo <> "src/Page", method = Left GET })
  liftEffect $ log $ "GET /api response: " <> J.stringify res.response

-- loadFiles :: J.Json -> Effect (Fiber (Array CheatData))
-- loadFiles = J.caseJson
--     (const []) -- null
--     (const []) -- bool
--     (const []) -- number
--     (const []) -- string
--     (\a -> map loadFile a) -- array
--     (\o -> [loadFile o] ) -- object



-- parseJsonResponse :: J.Json -> Maybe CheatData
-- parseJsonResponse = J.caseJson
--     (const Nothing) -- null
--     (const Nothing) -- bool
--     (const Nothing) -- number
--     (const Nothing) -- string 
--     (const Nothing) -- array
--     (Just <<< J.fromObject)  -- object

-- getCheatsheets :: GitDir -> Effect Unit
-- getCheatsheets dir = do
--    (Fiber (Just datas)) <- loadCheatsheetFiles
--    pure unit
   
-- loadFile :: GitData -> Effect (Fiber (Maybe CheatData))
-- loadFile json = launchAff $ do
--     res <- AX.affjax AXRes.json (AX.defaultRequest { url = json.url, method = Left GET })
--     pure $ eitherToMaybe $ parseCheatData $ J.stringify res.response

-- loadCheatsheetFiles :: GitDir -> Effect (Fiber (Maybe GitDatas))
-- loadCheatsheetFiles (GitDir dir) = launchAff $ do
--    res <-  AX.affjax AXRes.json (AX.defaultRequest { url = repo <> dir, method = Left GET })
--    pure $ eitherToMaybe $ parseGitData $ J.stringify res.response

-- eitherToMaybe :: forall a e. Either e a -> Maybe a
-- eitherToMaybe (Left _) = Nothing
-- eitherToMaybe (Right v) = Just v 