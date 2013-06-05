#!/usr/bin/env runhaskell

module Main where

import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Control.Monad.Identity
import Data.Maybe
import Data.List
import System.IO

data Osm a =
  Osm
    String -- version
    String -- generator
    String -- copyright
    String -- attribution
    String -- license
    a      -- value
  deriving (Eq, Show)

xpOsm ::
  PU a
  -> PU (Osm a)
xpOsm p =
  xpElem "osm" $
    xpWrap
      (
        \(v, g, c, a, l, x) -> Osm v g c a l x
      , \(Osm v g c a l x) -> (v, g, c, a, l, x)
      )
      (
        xp6Tuple
          (xpAttr "version" xpText0)
          (xpAttr "generator" xpText0)
          (xpAttr "copyright" xpText0)
          (xpAttr "attribution" xpText0)
          (xpAttr "license" xpText0)
          p
      )

data Way =
  Way
    String -- id
    String -- visible
    String -- timestamp
    String -- version
    String -- changeset
    String -- user
    String -- uid
    [String] -- nd ref
    [Tag] -- tag
    deriving (Eq, Show)

xpWay ::
  PU (Osm Way)
xpWay =
  xpOsm .
  xpElem "way" $
  xpWrap
    (
      (\(i, s, t, v, c, u, d, r, g) -> Way i s t v c u d r g)
    , (\(Way i s t v c u d r g) -> (i, s, t, v, c, u, d, r, g))
    )
    (
      xp9Tuple
        (xpAttr "id" xpText0)
        (xpAttr "visible" xpText0)
        (xpAttr "timestamp" xpText0)
        (xpAttr "version" xpText0)
        (xpAttr "changeset" xpText0)
        (xpAttr "user" xpText0)
        (xpAttr "uid" xpText0)
        (xpList (xpElem "nd" (xpAttr "ref" xpText0)))
        (xpList xpTag)
    )

data Tag =
  Tag
    String -- k
    String -- v
  deriving (Eq, Show)

xpTag ::
  PU Tag
xpTag =
  xpElem "tag" $
    xpWrap
      (
        (uncurry Tag)
      , (\(Tag k v) -> (k, v))
      )
      (
        xpPair
          (xpAttr "k" xpText0)
          (xpAttr "v" xpText0)
      )

pairTag ::
  Tag
  -> (String, String)
pairTag (Tag k v) =
  (k, v)

data Node =
  Node
    String -- id
    String -- version
    String -- changeset
    String -- lat
    String -- lon
    String -- user
    String -- uid
    String -- visible
    String -- timestamp
    [Tag]  -- tag
  deriving (Eq, Show)

xpNode ::
  PU (Osm Node)
xpNode =
  xpOsm .
  xpElem "node" $
  xpWrap
    (
      (\(i, v, c, t, n, u, d, e, p, g) -> Node i v c t n u d e p g)
    , (\(Node i v c t n u d e p g) -> (i, v, c, t, n, u, d, e, p, g))
    )
    (
      xp10Tuple
        (xpAttr "id" xpText0)
        (xpAttr "version" xpText0)
        (xpAttr "changeset" xpText0)
        (xpAttr "lat" xpText0)
        (xpAttr "lon" xpText0)
        (xpAttr "user" xpText0)
        (xpAttr "uid" xpText0)
        (xpAttr "visible" xpText0)
        (xpAttr "timestamp" xpText0)        
        (xpList xpTag)
    )

data Relation =
  Relation
    String -- id
    String -- visible
    String -- timestamp
    String -- version
    String -- changeset
    String -- user
    String -- uid
    [Member] -- member
    [Tag] -- tag
    deriving (Eq, Show)

xpRelation ::
  PU (Osm Relation)
xpRelation =
  xpOsm .
  xpElem "relation" $
  xpWrap
    (
      (\(i, s, t, v, c, u, d, m, g) -> Relation i s t v c u d m g)
    , (\(Relation i s t v c u d m g) -> (i, s, t, v, c, u, d, m, g))
    )
    (
      xp9Tuple
        (xpAttr "id" xpText0)
        (xpAttr "visible" xpText0)
        (xpAttr "timestamp" xpText0)
        (xpAttr "version" xpText0)
        (xpAttr "changeset" xpText0)
        (xpAttr "user" xpText0)
        (xpAttr "uid" xpText0)
        (xpList xpMember)
        (xpList xpTag)
    )

data Member =
  Member
    String -- type
    String -- ref
    String -- role
  deriving (Eq, Show)

xpMember ::
  PU Member
xpMember =
  xpElem "member" $
    xpWrap
      (
        (\(t, r, l) -> Member t r l)
      , (\(Member t r l) -> (t, r, l))
      )
      (
        xpTriple
          (xpAttr "type" xpText0)
          (xpAttr "ref" xpText0)
          (xpAttr "role" xpText0)
      )

data ApiConfig =
  ApiConfig
    String -- base uri
    String -- way uri
    String -- node uri
    String -- relation uri
    SysConfigList -- configuration list

config ::
  ApiConfig
config =
  ApiConfig
    "http://www.openstreetmap.org/api/0.6"
    "way"
    "node"
    "relation"
    [withRemoveWS yes, withCurl []]

data ApiConfigReaderT f a =
  ApiConfigReaderT
    (ApiConfig -> f (Either String a))

instance Functor f => Functor (ApiConfigReaderT f) where
  fmap f (ApiConfigReaderT k) =
    ApiConfigReaderT ((fmap . fmap) f . k)

instance Monad f => Monad (ApiConfigReaderT f) where
  return =
    ApiConfigReaderT . return . return . return
    
  ApiConfigReaderT k >>= f =
    ApiConfigReaderT (\c -> k c >>= either (return . Left) (\a -> let ApiConfigReaderT l = f a in l c))

type ApiConfigReader =
  ApiConfigReaderT Identity

apiConfigReader ::
  (ApiConfig -> Either String a)
  -> ApiConfigReader a
apiConfigReader f =
  ApiConfigReaderT (Identity . f)

(~>) ::
  ApiConfigReaderT f a
  -> ApiConfig
  -> f (Either String a)
(~>) (ApiConfigReaderT k) =
  k

(~>>) ::
  ApiConfigReader a
  -> ApiConfig
  -> Either String a
ApiConfigReaderT k ~>> c =
  runIdentity (k c)

way ::
  String
  -> ApiConfigReaderT IO (Osm Way)
way i =
  ApiConfigReaderT (\(ApiConfig u w _ _ l) -> 
    let a = concat [u, "/", w, "/", i]
    in fmap (\ts ->
              case ts of []    -> Left "No tree"
                         (t:_) -> unpickleDoc' xpWay t) . runX . readDocument l $ a)

node ::
  String
  -> ApiConfigReaderT IO (Osm Node)
node i =
  ApiConfigReaderT (\(ApiConfig u _ n _ l) -> 
    let a = concat [u, "/", n, "/", i]
    in fmap (\ts ->
              case ts of []    -> Left "No tree"
                         (t:_) -> unpickleDoc' xpNode t) . runX . readDocument l $ a)

relation ::
  String
  -> ApiConfigReaderT IO (Osm Relation)
relation i =
  ApiConfigReaderT (\(ApiConfig u _ _ r l) -> 
    let a = concat [u, "/", r, "/", i]
    in fmap (\ts ->
              case ts of []    -> Left "No tree"
                         (t:_) -> unpickleDoc' xpRelation t) . runX . readDocument l $ a)

nodes ::
  String
  -> ApiConfigReaderT IO (Osm Way, [Osm Node])
nodes i =
  do o@(Osm _ _ _ _ _ (Way _ _ _ _ _ _ _ n _)) <- way i
     e <- mapM node n 
     return (o, e)
     
way2Trk ::
  Osm Way
  -> [Osm Node]
  -> [Char]
way2Trk (Osm _ _ _ _ _ (Way z _ _ _ _ _ _ _ g)) n =
     let g' = map pairTag g
         name = lookup "name" g'
         hway = lookup "highway" g'
         sfce = lookup "surface" g'
         srnd e q = concat ["<", e, ">", q, "</", e, ">"] 
     in intercalate "\n" $
       [
         "<trk>"
       , concatMap (srnd "name") . maybeToList $ name
       , concatMap (\h -> srnd "desc" (h ++ " highway" ++ maybe [] (\sc -> concat [" (", sc, ")"]) sfce)) . maybeToList $ hway
       , srnd "src" z
       , "<trkseg>"
       , n >>= (\(Osm _ _ _ _ _ (Node _ _ _ lat lon _ _ _ _ _ )) -> concat ["<trkpt lat=\"", lat, "\" lon=\"", lon, "\"/>", "\n"])
       , "</trkseg>"
       , "</trk>"
       ]

way2Trks ::
  [(Osm Way, [Osm Node])]
  -> [Char]
way2Trks =
  intercalate "\n" . map (uncurry way2Trk)

gpx ::
  [String]
  -> ApiConfigReaderT IO String
gpx z =
  do ts <- mapM (\q -> nodes q) z
     return . intercalate "\n" $
       [
         gpxHead
       , way2Trks ts
       , "</gpx>"
       ]

gpxHead ::
  String
gpxHead =
  "\
\<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
\<gpx\n\
\ version=\"1.0\"\n\
\  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n\
\ xmlns=\"http://www.topografix.com/GPX/1/0\"\n\
\ xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n\
\<metadata>\n\
\  <name>Trans-Australia Ride of 2014</name>\n\
\  <desc>Route and waypoints</desc>\n\
\  <author>\n\
\    <name>Tony Morris</name>\n\
\    <email id=\"code\" domain=\"tmorris.net\"/>\n\
\    <link href=\"http://tmorris.net/\">\n\
\      <text>Tony Morris</text>\n\
\      <type>HTTP</type>\n\
\    </link>\n\
\  </author>\n\
\  <copyright author=\"Tony Morris\">\n\
\    <year>2013</year>\n\
\    <license>http://en.wikipedia.org/wiki/Beerware</license>\n\
\  </copyright>\n\
\  <link href=\"http://www.ride.tmorris.net/\">\n\
\    <text>Trans-Australia Ride of 2014</text>\n\
\    <type>HTTP</type>\n\
\  </link>\n\
\</metadata>\n\
\"

ways ::
  [String]
ways =
  [
    "186473280" -- Birdsville Development Road
  , "186473277" -- Birdsville Development Road
  ]

main ::
  IO ()
main =
  do r <- gpx ways ~> config
     case r of Left e -> hPutStrLn stderr e
               Right g -> putStrLn g
