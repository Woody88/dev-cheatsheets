module Router 
    ( Route(..)
    , Page(..)
    , parseRoute
    , routing
    )
    where
  
import Prelude

import Data.String (toLower)
import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Maybe (fromMaybe)
import Data.String (stripPrefix, Pattern(..))
import Routing (match)
import Routing.Match (Match, lit, root, end)

data Route
    = Home
    | Cheatsheet Page
    | NotFound

data Page 
    = Purescript
    | Haskell 
    | Salesforce
    | Bash 

derive instance routeEq :: Eq Route
derive instance pageEq :: Eq Page

instance showPages :: Show Page where
  show (Purescript) = "Purescript"
  show (Haskell)    = "Haskell"
  show (Salesforce) = "Salesforce"
  show (Bash)       = "Bash"
      
instance showRoutes :: Show Route where
  show (Home)        = "/"
  show (Cheatsheet p) = show p
  show (NotFound)    = "404"

parseRoute :: String -> Route
parseRoute = parseRouteEither <<< match routing <<< stripPrefixBackslash
    where parseRouteEither = either (const NotFound) identity
          stripPrefixBackslash path = fromMaybe (show NotFound) $ stripPrefix (Pattern "#") path

routing :: Match Route
routing = oneOf
    [ home
    , purescript
    , haskell
    , salesforce
    , bash
    , pure NotFound
    ]
    where 
          home = Home <$ root <* end
          purescript = (Cheatsheet Purescript) <$ lit (showLowerCase Purescript) <* end
          haskell = (Cheatsheet Haskell) <$ lit (showLowerCase Haskell) <* end
          salesforce = (Cheatsheet Salesforce) <$ lit (showLowerCase Salesforce) <* end
          bash = (Cheatsheet Bash) <$ lit (showLowerCase Bash) <* end
          showLowerCase = toLower <<< show  
        


      