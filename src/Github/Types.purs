module Github.Types where
    
import Data.Either (Either) 
import Simple.JSON (readJSON)
import Data.List.Types (NonEmptyList)
import Foreign (ForeignError)


newtype GitDir = GitDir String

type GitDatas = Array GitData
type GitData = { url :: String }

type CheatData = 
    { name   :: String
    , content :: String
    }

parseGitData :: String -> Either (NonEmptyList ForeignError) GitDatas 
parseGitData s = readJSON s

parseCheatData :: String -> Either (NonEmptyList ForeignError) CheatData 
parseCheatData s = readJSON s

repo :: String
repo = "https://api.github.com/repos/Woody88/dev-cheatsheets/contents/Cheatsheets/"
