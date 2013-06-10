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

mkRoute ::
  [Coord]
  -> Route
mkRoute c =
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

boreTrackRoute ::
  Route  
boreTrackRoute =
  concat
  [
    [
      brisbaneBegin
    , charleville
    , cordilloRoadNorth
    , innamincka
    , boreTrackNorth
    , boreTrackSouth
    , oodnadatta
    , aliceSprings
    ]
  , darwin2BrisbaneViaRockhampton
  ] @.
  (Z5, lambert)

darwinRoute ::
  Route
darwinRoute =
  concat
  [
    [
      brisbaneBegin
    , charleville
    , birdsville
    , poepellCorner
    , purniBore
    , dalhousie
    , oodnadatta
    , aliceSprings
    , darwin
    ]
  , savannahKatherine2Cairns
  , [
      townsville
    , herveyBay
    , brisbaneBegin
    ]
  ] @.
  (Z5, lambert)

route ::
  Route
route =
  let brisbane2Oodnadatta =
        [
          brisbaneBegin
        , charleville
        , birdsville
        , poepellCorner
        , purniBore
        , dalhousie
        , oodnadatta
        ]
      oodnadatta2Darwin =
        [
          aliceSprings
        , jabiru
        , darwin
        ]
      darwin2Cairns =
        [
          katherine
        , matarankaFalls
        , -14.724925 |. 134.501689
        , battenCreek
        , -16.143237 |. 136.086945
        , borroloola
        , -17.215699 |. 137.938498 -- wollogorang
        , -17.936602 |. 138.83096 -- doomadgee
        , burketown
        , croydon
        , cairns
        ]
      cairns2Brisbane =
        [
          townsville
        , herveyBay
        , brisbaneBegin
        ]
  in concat
       [
         brisbane2Oodnadatta
       , oodnadatta2Darwin
       , darwin2Cairns
       , cairns2Brisbane
       ] @.
      (Z5, longreach)

routes ::
  [(Route, String)]
routes =
  [
    (boreTrackRoute, "Brisbane, Charleville, Cordilo Road, Innamincka, Bore Track (alt to Strzelecki), Lyndhurst, Oodnadatta, Alice Springs, Jabiru, Darwin, McArthur, Mount Isa, Rockhampton, Hervey Bay, Brisbane")
  , (darwinRoute    , "Brisbane, Birdsville, Purni Bore, Oodnadatta, Alice Springs, Jabiru, Darwin, McArthur, Cairns, Townsville, Hervey Bay, Brisbane")
  , (route          , "")
  ]

main ::
  IO ()
main =
  mapM_ (\(r, n) -> putStrLn (concat [r ~> config, " (", n, ")"])) routes


lambert ::
  Coord
lambert =
  -25.610111 |. 134.354806

brisbaneBegin ::
  Coord
brisbaneBegin =
  -27.688460 |. 153.183840

subiaco ::
  Coord
subiaco =
  -31.944690 |. 115.823720

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
  -27.747408 |. 140.736025

birdsville ::
  Coord
birdsville =
  -25.897179 |. 139.355546

blinman :: -- Flinders Ranges
  Coord 
blinman =
  -31.09548 |. 138.678807

boreTrackNorth ::
  Coord
boreTrackNorth =
  -27.756629 |. 140.900438

boreTrackMiddle ::
  Coord
boreTrackMiddle =
  -27.82124 |. 140.854602

boreTrackSouth ::
  Coord
boreTrackSouth =
  -28.130879 |. 140.911875

birdsvilleTrackNorth ::
  Coord
birdsvilleTrackNorth =
  -25.918503 |. 139.378758

birdsvilleInsideTrackNorth ::
  Coord
birdsvilleInsideTrackNorth =
  -25.911752 |. 139.333197

birdsvilleInsideTrackSouth ::
  Coord
birdsvilleInsideTrackSouth =
  -26.998051 |. 139.024969

warakurna ::
  Coord
warakurna =
  -25.053280 |. 128.299351

warburton ::
  Coord
warburton =
  -26.133004 |. 126.58064

tjukayirla ::
  Coord
tjukayirla =
  -27.153786 |. 124.573898

erldunda ::
  Coord
erldunda =
  -25.198952 |. 133.201952

cosmoNewberry ::
  Coord
cosmoNewberry =
  -27.994930 |. 122.89516

laverton ::
  Coord
laverton =
  -28.6266277 |. 122.403518

dockerRiver ::
  Coord
dockerRiver =
  -24.8687942 |. 129.095428

oldGunbarrelNorth ::
  Coord
oldGunbarrelNorth =
  -25.707433 |. 126.664721

oldGunbarrelSouth ::
  Coord
oldGunbarrelSouth =
  -25.971631 |. 126.800923

norseman ::
  Coord
norseman =
  -32.185311 |. 121.778508

esperance ::
  Coord
esperance =
  -33.832102 |. 121.896204

ravensthorpe ::
  Coord
ravensthorpe =
  -33.576084 |. 120.012066

hyden ::
  Coord
hyden =
  -32.450172 |. 118.86318

kondinin ::
  Coord
kondinin =
  -32.499175 |. 118.264772

wickepin ::
  Coord
wickepin =
  -32.78161 |. 117.496412

bunburry ::
  Coord
bunburry =
  -33.350606 |. 115.696242

busselton ::
  Coord
busselton =
  -33.663639 |. 115.359178

poepellCorner ::
  Coord
poepellCorner =
  -26.004933 |. 137.991865

purniBore ::
  Coord
purniBore =
  -26.284170 |. 136.098330

dalhousie ::
  Coord
dalhousie =
  -26.423608 |. 135.502891

oodnadatta ::
  Coord
oodnadatta =
  -27.548276 |. 135.448427

aliceSprings ::
  Coord
aliceSprings =
  -23.700651 |. 133.876855

hallsCreek ::
  Coord
hallsCreek =
  -18.222707 |. 127.670181

jabiru ::
  Coord
jabiru =
  -12.654073 |. 132.82907

darwin ::
  Coord
darwin =
  -12.462852 |. 130.841843

townsville ::
  Coord
townsville =
  -19.257653 |. 146.817809

mcarthur ::
  Coord
mcarthur =
  -16.682342 |. 135.727454

cairns ::
  Coord
cairns =
  -16.920154 |. 145.771015

herveyBay ::
  Coord
herveyBay =
  -25.289521 |. 152.830889

fortyMileScrub ::
  Coord
fortyMileScrub =
  -18.10837 |. 144.826121

mountIsa :: 
  Coord
mountIsa =
  -20.724548 |. 139.494714

rockhampton ::
  Coord
rockhampton =
  -23.322159 |. 150.512252

katherine ::
  Coord
katherine =
  -14.463563 |. 132.260835

ngukurr ::
  Coord
ngukurr =
  -14.739442 |. 134.733016

burketown ::
  Coord
burketown =
  -17.741084 |. 139.547989

croydon ::
  Coord
croydon =
  -18.203996 |.142.244661

longreach ::
  Coord
longreach =
  -23.437537 |. 144.24207

matarankaFalls ::
  Coord
matarankaFalls =
  -14.955161 |. 133.21806

battenCreek ::
  Coord
battenCreek =
  -16.30139 |. 135.70722

borroloola ::
  Coord
borroloola =
  -16.088083 |. 136.305138

greatCentralRoad ::
  [Coord]
greatCentralRoad =
  [dockerRiver, laverton]

darwin2BrisbaneViaCairns ::
  [Coord]
darwin2BrisbaneViaCairns =
  [
    darwin
  , mcarthur
  , fortyMileScrub
  , cairns
  , townsville
  , herveyBay
  , brisbaneBegin
  ]

darwin2BrisbaneViaRockhampton ::
  [Coord]
darwin2BrisbaneViaRockhampton =
  [
    darwin
  , jabiru
  , mcarthur
  , mountIsa
  , rockhampton
  , brisbaneBegin
  ]

savannahKatherine2Cairns ::
  [Coord]
savannahKatherine2Cairns =
  [
    katherine
  , ngukurr
  , burketown
  , croydon
  , cairns
  ]

-- -16.04338,142.38641 (dunbar)
-- -16.062393,143.032243
-- -15.971685,143.354541
-- -16.476749,143.599502 (gamboola)
-- -16.685657,144.652272 (nychum)
-- -16.925583,145.037897 (thornborough)
