{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Routing.Core
  ( Router
  , Location(..)
  , (:~)(..)
  , (>>>), (<<<)
  , transform
  , convertRoute
  , end
  , generate
  , int
  , lit
  , many
  , many1
  , tailer
  , takeExactly
  , recognize
  , routeData
  , scanner
  , section
  ) where

import            Prelude hiding (id, (.))

import            Control.Applicative hiding (many)
import            Control.Category
import            Data.Char
import            Data.Convertible
import            Data.Data
import qualified  Data.List as List
import            Data.Maybe (listToMaybe)

data a :~ b = a :~ b
  deriving (Eq, Show, Data, Typeable)

getConstructorParams :: (Data a, Typeable p)
                     => (p -> a) -> a -> Maybe p
getConstructorParams constructor value = do
  realParams <- gmapQi 0 cast value
  let dummyValue = constructor realParams

  if getConstrIndex dummyValue == getConstrIndex value then
    return realParams
  else
    Nothing

getConstrIndex :: Data a => a -> Int
getConstrIndex = constrIndex . toConstr

data Location dat = Location {
    locationPath :: String
  , locationData :: Maybe dat
  }

emptyLocation :: Location dat
emptyLocation = Location "" Nothing

modifyPath :: (String -> String) -> Location dat -> Location dat
modifyPath f loc = loc { locationPath = f (locationPath loc) }

modifyData :: (Maybe dat -> Maybe dat) -> Location dat -> Location dat
modifyData f loc = loc { locationData = f (locationData loc) }

data Router dat a b = Router {
    rParse :: a -> Location dat -> Maybe (b, Location dat)
  , rGenerate :: b -> Location dat -> Maybe (a, Location dat)
  }

transform :: (b -> Maybe c)
          -> (c -> Maybe b)
          -> Router dat a b
          -> Router dat a c
transform extract inject router = Router {
    rParse = \a loc -> do (b, loc') <- rParse router a loc
                          c <- extract b
                          pure (c, loc')

  , rGenerate = \c loc -> do b <- inject c
                             rGenerate router b loc
  }

convertRoute :: (Convertible c b, Convertible b c)
             => Router dat a b
             -> Router dat a c
convertRoute =
    transform tryConvert tryConvert
  where
    tryConvert a = either (const Nothing) Just (safeConvert a)

recognize :: Location dat -> Router dat () b -> Maybe b
recognize path router = fst <$> rParse router () path

generate :: b -> Router dat () b -> Maybe (Location dat)
generate b router = snd <$> rGenerate router b emptyLocation

idRouter :: Router dat a a
idRouter = Router {
    rParse = \_ _ -> Nothing
  , rGenerate = \_ _ -> Nothing
  }

composeRoute :: Router dat b c -> Router dat a b -> Router dat a c
composeRoute bc ab = Router {
    rParse = \a path ->
              case rParse ab a path of
              Nothing -> Nothing
              Just (b, newPath) -> rParse bc b newPath

  , rGenerate = \c path ->
                  case rGenerate bc c path of
                  Nothing -> Nothing
                  Just (b, newPath) -> rGenerate ab b newPath
  }

routerPlus :: Router dat a b -> Router dat a b -> Router dat a b
routerPlus r1 r2 = Router {
    rParse = \a path -> rParse r1 a path <|> rParse r2 a path
  , rGenerate = \b path -> rGenerate r1 b path <|> rGenerate r2 b path
  }

routerZero :: Router dat a b
routerZero = Router {
    rParse = \_ _ -> Nothing
  , rGenerate = \_ _ -> Nothing
  }

lit :: String -> Router dat a a
lit str = Router {
    rParse = \a location ->
              if List.isPrefixOf str (locationPath location) then
                Just (a, modifyPath (drop $ length str) location)
              else
                Nothing

  , rGenerate = \b location -> Just (b, modifyPath (str++) location )
  }

end :: (Typeable a, Data b) => (a -> b) -> Router dat a b
end constructor = Router {
    rParse = \a location -> if null (locationPath location)
                            then Just (constructor a, location)
                            else Nothing

  , rGenerate = \b location ->
                  case getConstructorParams constructor b of
                  Just params -> Just (params, location)
                  Nothing -> Nothing
  }

scanner :: (Char -> Bool)
        -> (String -> Maybe b)
        -> (b -> String)
        -> Router dat a (a :~ b)
scanner cond reader shower = Router { rParse = prs, rGenerate = gen }
  where prs a location =
          let chars = takeWhile cond (locationPath location)
              charCount = length chars
          in if charCount > 0 then do
                value <- reader chars
                return (a :~ value, modifyPath (drop charCount) location)
             else
                Nothing

        gen (a :~ b) location = return (a, modifyPath (shower b ++) location)

int :: Router dat a (a :~ Int)
int = scanner isDigit reader show
  where reader = fmap fst . listToMaybe . reads

section :: Router dat a (a :~ String)
section = scanner (not . (== '/')) Just id

tailer :: Router dat a (a :~ String)
tailer = scanner (not . (== '&')) Just show

takeExactly :: Int -> Router dat a (a :~ String)
takeExactly n = Router {
    rParse = \a location ->
                let (part, rest) = splitAt n (locationPath location)
                in if length part == n
                   then Just (a :~ part, modifyPath (const rest) location)
                   else Nothing

  , rGenerate = \(a :~ str) location ->
                  if length str == n
                  then Just (a, modifyPath (++str) location)
                  else Nothing
  }

routeData :: (Maybe dat -> Maybe dat)
          -> (Maybe dat -> Maybe dat)
          -> Router dat a a
routeData parse gen = Router {
    rParse = \a location ->
               case parse (locationData location) of
               Nothing -> Nothing
               Just dat -> Just (a, modifyData (const (Just dat)) location)

  , rGenerate = \a location ->
                 case gen (locationData location) of
                 Nothing -> Nothing
                 Just dat -> Just (a, modifyData (const (Just dat)) location)
  }

many :: Router dat a (a :~ b) -> Router dat a (a :~ [b])
many router = Router (parse []) gen
  where parse items a location =
          case rParse router a location of
          Nothing -> Just (a :~ items, location)
          Just (_ :~ b, location') -> parse (b:items) a location'

        gen (a :~ []) location = Just (a, location)

        gen (a :~ (b:rest)) location =
          case rGenerate router (a :~ b) location of
          Nothing -> Nothing
          Just (_, location') -> gen (a :~ rest) location'


many1 :: Router dat a (a :~ b) -> Router dat a (a :~ [b])
many1 router = transform extract inject (many router)
  where extract (_ :~ []) = Nothing
        extract params = Just params

        inject (_ :~ []) = Nothing
        inject params = Just params

-- instances

instance Convertible b c => Convertible (a :~ b) (a :~ c) where
  safeConvert (a :~ b) = (a :~ ) <$> safeConvert b

instance Category (Router dat) where
  id = idRouter
  (.) = composeRoute

instance Monoid (Router dat a b) where
  mempty = routerZero
  mappend = routerPlus

