module SimpleOsRelease (
  readOsRelease,
  parseOsReleaseLine
  )
where

import Data.Either (partitionEithers)
import Data.List.Extra
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)


-- | unsafe read and parse of os-release file
readOsRelease :: IO [(String,String)]
readOsRelease = do
  (errs,oss) <- partitionEithers <$> eitherReadOsRelease
  if null errs
    then return oss
    else error $ "malformed os-release file:" ++ unlines errs

-- | safe read and parse of os-release
eitherReadOsRelease :: IO [Either String (String,String)]
eitherReadOsRelease = do
  file <- do
    let etcfile = "/etc/os-release"
    etc <- doesFileExist etcfile
    if etc
      then return etcfile
      else do
      let libfile = "/usr/lib/os-release"
      lib <- doesFileExist libfile
      if lib
        then return libfile
        else error "could not find os-release file"
  osrelease <- readFile file
  return $ mapMaybe parseOsReleaseLine $ lines osrelease

-- | parse an os-release line
parseOsReleaseLine :: String -> Maybe (Either String (String,String))
parseOsReleaseLine l =
  case trim l of
    "" -> Nothing
    ('#':_) -> Nothing
    ls ->
      Just $
      case breakOn "=" ls of
        ("",_) -> Left $ "ill-formed line: " ++ l
        (k,('=':v)) ->
          let val =
                dropSuffix "\"" $
                dropPrefix "\"" v
          in Right (k,val)
        (_,_) -> Left $ "ill-formed line: " ++ l
