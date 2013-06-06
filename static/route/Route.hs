#!/usr/bin/env runhaskell

module Main where

import Data.List

data Coord =
  Coord 
    Double -- lat
    Double -- lon
  deriving Eq

(|.) ::
  Double
  -> Double
  -> Coord
(|.) =
  Coord

infixr 4 |.

instance Show Coord where
  show (Coord lat lon) =
    concat ["(", show lat, ",", show lon, ")"]

data Zoom =
  Z0
  | Z1
  | Z2
  | Z3
  | Z4
  | Z5
  | Z6
  | Z7
  | Z8
  | Z9
  | Z10
  | Z11
  | Z12
  | Z13
  | Z14
  | Z15
  | Z16
  | Z17
  | Z18
  deriving (Eq, Enum)

instance Show Zoom where
  show =
    show . fromEnum

data Route =
  Route
    [Coord]
    (Maybe (Zoom, Coord))   -- zoom & centre

route ::
  [Coord]
  -> Route
route c =
  Route c Nothing

(@.) ::
  [Coord]
  -> (Zoom, Coord)
  -> Route
c @. z =
  Route c (Just z)

infixr 4 @.    

data Config =
  Config
    String -- base uri
    String -- location parameter
    String -- zoom parameter
    String -- centre parameter
  deriving Eq

config ::
  Config
config =
  Config
    "http://map.project-osrm.org/"
    "loc"
    "z"
    "center"

(~>) ::
  Route
  -> Config
  -> String
Route c z ~> Config uri loc zoom centre =
  let locs = intercalate "&" . map (\(Coord lat lon) -> concat [loc, "=", show lat, ",", show lon]) $ c
      z' = maybe [] (\(zz, Coord lat lon) -> concat ["&", zoom, "=", show zz, "&", centre, "=", show lat, ",", show lon]) z      
  in concat [uri, "?", locs, z']

brisbaneToPerth ::
  Route
brisbaneToPerth =  
  [
    -27.688460 |. 153.183840
  , -31.944690 |. 115.823720
  ] @.
  (Z5, lambert)
    
strzelecki ::
  Route  
strzelecki =
  [
    -27.688460 |. 153.183840
  , charleville
  , cordilloRoadNorth
  , innamincka
  , -31.944690 |. 115.823720
  ] @.
  (Z5, lambert)

routes ::
  [(Route, String)]
routes =
  [
    (brisbaneToPerth, "Brisbane to Perth")
  , (strzelecki     , "Strzelecki")
  ]

main ::
  IO ()
main =
  mapM_ (\(r, n) -> putStrLn (concat [r ~> config, " (", n, ")"])) routes


lambert ::
  Coord
lambert =
  -25.610111 |. 134.354806

charleville ::
  Coord
charleville =
  -26.404041 |. 146.249285

cordilloRoadNorth ::
  Coord
cordilloRoadNorth =
  -25.777314 |. 140.347091

innamincka ::
  Coord
innamincka =
  -27.707581 |. 140.739217

birdsville ::
  Coord
birdsville =
  -25.897179 |. 139.355546