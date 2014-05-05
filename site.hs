{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid(mappend, mconcat)
import           Hakyll
import           Data.Maybe(fromMaybe)
import qualified Data.Map as M
import           Text.Pandoc(WriterOptions, HTMLMathMethod(..), writerHTMLMathMethod)
import           System.FilePath(combine, dropExtension)
import           Control.Monad.Error.Class
import           Control.Arrow(second)
import           Control.Applicative((<$>))
import           Control.Lens((.~))
import           Data.Geo.Route(Waypoint, Track, Plan, mkPlan', mkAuthor', mkEmail, mkLink, mkCopyright', gpx, osrm, trackHeader, mkTrackHeader', (<@>), (.<.>), (<~>), (<.?>), (|.|), (.|))
import           Data.Geo.Coordinate((..#..))
import Data.String


main ::
  IO ()
main =
  hakyllWith configuration $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/**" $ do
        route   (gsubRoute "static/" (const ""))
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match (fromList ["contact.markdown", "404.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match "posts/*" $ do
        route $ customRoute fileToDirectory
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= saveSnapshot "content"
            -- >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/disqus.html"  (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            let archiveCtx =
                    constField "posts" list                    `mappend`
                    constField "title" "Posts"                 `mappend`
                    postCtx tags

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create [fromString ridegpx] $ do
        route idRoute
        compile $
            case plan of
              Just p -> makeItem (gpx p) >>= relativizeUrls
              Nothing -> throwError ["Plan cannot be determined."]

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"
                        (constField "title" title   `mappend`
                            constField "posts" list `mappend`
                            constField "tag" tag `mappend`
                            field "tags" (\_ -> renderTagList tags) `mappend`
                            defaultCtx)
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls
        version "atom" $ do
            route $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 5) . recentFirst
            let indexCtx = constField "posts" list          `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- Render RSS feed
    create ["atom.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots "posts/*" "content"
                >>= recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx
    
colourField ::
  String
  -> String
  -> Context a
colourField name defaultC = field name $ \i -> do
  metadata <- getMetadata (itemIdentifier i)
  return . fromMaybe defaultC . M.lookup "colour" $ metadata

defaultColour ::
  String
defaultColour =
  "light cyan"

postCtx ::
  Tags
  -> Context String
postCtx tags =
  mconcat
    [ 
      tagsField  "tags"  tags
    , dateField  "date"  "%B %e, %Y"
    , constField "ride.gpx" ridegpx
    , constField "osrm" (case plan of
                           Just p -> osrm p
                           Nothing -> "Plan cannot be determined.")
    , defaultCtx
    ]

defaultCtx ::
  Context String
defaultCtx =
  mconcat
    [
      colourField "colour" defaultColour
    , constField "ride.gpx" ridegpx
    , constField "osrm" (case plan of
                           Just p -> osrm p
                           Nothing -> "Plan cannot be determined.")
    , defaultContext
    ]

postList ::
  Tags
  -> Pattern
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList tags pattern sortFilter = do
  posts   <- sortFilter =<< loadAll pattern
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl (postCtx tags) posts

feedCtx ::
  Context String
feedCtx =
  mconcat
    [ 
      bodyField "description"
    , defaultContext
    ]

feedConfiguration ::
  String
  -> FeedConfiguration
feedConfiguration title =
  FeedConfiguration
    { 
      feedTitle       = "Ride 2014 Planning — " ++ title
    , feedDescription = "Planning for the Trans-Australia Ride of 2014"
    , feedAuthorName  = "Tony Morris"
    , feedAuthorEmail = "ride@tmorris.net"
    , feedRoot        = "http://ride.tmorris.net"
    }

pandocOptions ::
  WriterOptions
pandocOptions =
  defaultHakyllWriterOptions {
    writerHTMLMathMethod = MathJax ""
  }

configuration ::
  Configuration
configuration =
  defaultConfiguration

fileToDirectory ::
  Identifier
  -> FilePath
fileToDirectory =
  flip combine "index.html" . dropExtension . uncurry (++) . second (drop 11 {- yyyy-mm-dd- -}) . splitAt 6 {- "posts/" -} . toFilePath

----

bourke ::
  Maybe Waypoint
bourke =
  do x <- (-30.088869) ..#.. 145.937757
     return ("City (Medium)" <@> "Bourke, NSW" .<.> x)

warregoHotel ::
  Maybe Waypoint
warregoHotel =
  do x <- (-29.752314) ..#.. 145.425183
     return ("Lodging" <@> "Warrego Hotel" .<.> x)

currawinyaNP ::
  Maybe Waypoint
currawinyaNP =
  do x <- (-28.868504) ..#.. 144.468484
     return ("Forest" <@> "Currawinya NP, QLD" .<.> x)

thargomindah ::
  Maybe Waypoint
thargomindah =
  do x <- (-27.99173) ..#.. 143.819636
     return ("City (Small)" <@> "Thargomindah, QLD" .<.> x)

innamincka ::
  Maybe Waypoint
innamincka =
  do x <- (-27.70761) ..#.. 140.73925
     return ("City (Small)" <@> "Innamincka, SA" .<.> x)

oldStrzleckiNorthEnd ::
  Maybe Waypoint
oldStrzleckiNorthEnd =
  do x <- (-28.08943) ..#.. 140.56706
     return ("Pin, Green" <@> "End of Old Strzelecki Track (north)" .<.> x)

oldStrzleckiSouthStart ::
  Maybe Waypoint
oldStrzleckiSouthStart =
  do x <- (-28.0904) ..#.. 140.5607
     return ("Pin, Green" <@> "Start of Old Strzelecki Track (south)" .<.> x)

mertyMerty ::
  Maybe Waypoint
mertyMerty =
  do x <- (-28.575402) ..#.. 140.290149
     return ("City (Small)" <@> "Merty Merty, SA" .<.> x)

oldStrzeleckiMertyMertyCameronCorner ::
  Maybe Waypoint
oldStrzeleckiMertyMertyCameronCorner =
  do x <- (-28.595917) ..#.. 140.275617
     return ("Pin, Green" <@> "Old Strzelecki Track/Merty Merty,Cameron Corner Road intersection" <~> "Old Strzelecki Track" .<.> x)

mertyMertyCameronCornerStrzelecki ::
  Maybe Waypoint
mertyMertyCameronCornerStrzelecki =
  do x <- (-28.564427) ..#.. 140.186559
     return ("Pin, Green" <@> "Merty Merty,Cameron Corner Road/Strzelecki Track intersection" <~> "Merty Merty,Cameron Corner Road" .<.> x)

lyndhurst ::
  Maybe Waypoint
lyndhurst =
  do x <- (-30.287457) ..#.. 138.3497
     return ("City (Medium)" <@> "Lyndhurst, SA" .<.> x)

borefieldRoadTurn ::
  Maybe Waypoint
borefieldRoadTurn =
  do x <- (-29.593370) ..#.. 137.380702
     return ("Pin, Green" <@> "Borefield Road turn" .<.> x)

olympicDam ::
  Maybe Waypoint
olympicDam =
  do x <- (-30.482769) ..#.. 136.891974
     return ("City (Medium)" <@> "Olympic Dam" .<.> x)

roxbyDowns ::
  Maybe Waypoint
roxbyDowns =
  do x <- (-30.564061) ..#.. 136.895607
     return ("City (Medium)" <@> "Roxby Downs" .<.> x)

purpleDownsTurn ::
  Maybe Waypoint
purpleDownsTurn =
  do x <- (-30.797447) ..#.. 136.914414
     return ("Pin, Green" <@> "Purple Downs turn" .<.> x)

billaKalina ::
  Maybe Waypoint
billaKalina =
  do x <- (-29.917052) ..#.. 136.187145
     return ("Residence" <@> "Billa Kalina" .<.> x)

mountEba ::
  Maybe Waypoint
mountEba =
  do x <- (-30.180817) ..#.. 135.664534
     return ("City (Small)" <@> "Mount Eba" .<.> x)

oldStuartHighwayStuartHighwayIntersection ::
  Maybe Waypoint
oldStuartHighwayStuartHighwayIntersection =
  do x <- (-29.915912) ..#.. 135.154241
     return ("Pin, Green" <@> "Old Stuart/Stuart Highway" .<.> x)

cooberPedy ::
  Maybe Waypoint
cooberPedy =
  do x <- (-29.013368) ..#.. 134.753616
     return ("City (Medium)" <@> "Coober Pedy" .<.> x)

pukatjaKenmoreRdTurn ::
  Maybe Waypoint
pukatjaKenmoreRdTurn =
  do x <- (-26.793238) ..#.. 133.326859
     return ("Pin, Green" <@> "Pukatja to Kenmore Rd Turn" <~> "Pukatja to Kenmore Rd Turn" .<.> x)

mulgaParkPukatjaRdTurn ::
  Maybe Waypoint
mulgaParkPukatjaRdTurn =
  do x <- (-26.273114) ..#.. 132.13978
     return ("Pin, Green" <@> "Mulga Park - Pukatja Rd Turn" .<.> x)

gunbarrelHighwayTurn ::
  Maybe Waypoint
gunbarrelHighwayTurn =
  do x <- (-25.976711) ..#.. 132.260147
     return ("Pin, Green" <@> "Mulga Park - Pukatja Rd Turn" .<.> x)

mulgaParkRd ::
  Maybe Waypoint
mulgaParkRd =
  do x <- (-25.909042) ..#.. 131.668527
     return ("Pin, Green" <@> "Mulga Park Rd" .<.> x)

mountConner ::
  Maybe Waypoint
mountConner =
  do x <- (-25.495612) ..#.. 131.899259
     return ("Scenic Area" <@> "Mount Conner" .<.> x)

darwin ::
  Maybe Waypoint
darwin =
  do x <- (-12.454617) ..#.. 130.839195
     return ("City (Large)" <@> "Darwin" .<.> x)

jabiru ::
  Maybe Waypoint
jabiru =
  do x <- (-12.668414) ..#.. 132.832214
     return ("City (Small)" <@> "Jabiru" .<.> x)

borroloola ::
  Maybe Waypoint
borroloola =
  do x <- (-16.088042) ..#.. 136.30514
     return ("City (Small)" <@> "Borroloola" .<.> x)

normanton ::
  Maybe Waypoint
normanton =
  do x <- (-17.673383) ..#.. 141.075089
     return ("City (Small)" <@> "Normanton" .<.> x)

minnamoolka ::
  Maybe Waypoint
minnamoolka =
  do x <- (-18.145526) ..#.. 144.779863
     return ("City (Small)" <@> "Minnamoolka" .<.> x)

chartersTowers ::
  Maybe Waypoint
chartersTowers =
  do x <- (-20.066977) ..#.. 146.259513
     return ("City (Medium)" <@> "Charters Towers" .<.> x)

dayboro ::
  Maybe Waypoint
dayboro =
  do x <- (-27.197121) ..#.. 152.822739
     return ("City (Small)" <@> "Dayboro" .<.> x)

trk ::
  Maybe Track
trk =
  let header = mkTrackHeader' "Trans-Australia ride of 2014" "Trans-Australia ride of 2014" "Trans-Australia ride of 2014"
  in (trackHeader .~ header) <$>
     dayboro |.|
     bourke |.|
     warregoHotel |.|
     currawinyaNP |.|
     (-28.291734) <.?> 143.861728 |.|
     thargomindah |.|
     innamincka |.|
     oldStrzleckiNorthEnd |.|
     oldStrzleckiSouthStart |.|
     mertyMerty |.|
     oldStrzeleckiMertyMertyCameronCorner |.|
     mertyMertyCameronCornerStrzelecki |.|
     lyndhurst |.|
     borefieldRoadTurn |.|
     olympicDam |.|
     roxbyDowns |.|
     purpleDownsTurn |.|
     (-30.427641) <.?> 136.399427 |.|
     billaKalina |.|
     mountEba |.|
     (-30.177584) <.?> 135.648081 |.|
     oldStuartHighwayStuartHighwayIntersection |.|
     cooberPedy |.|
     pukatjaKenmoreRdTurn |.|
     mulgaParkPukatjaRdTurn |.|
     gunbarrelHighwayTurn |.|
     mulgaParkRd |.|
     mountConner |.|
     darwin |.|
     jabiru |.|
     borroloola |.|
     normanton |.|
     minnamoolka |.|
     chartersTowers .|
     dayboro

plan ::
  Maybe Plan
plan =
  fmap (
    mkPlan'
      "Trans-Australia Ride of 2014"
      "Trans-Australia Ride of 2014"
      (
        mkAuthor'
          "Tony Morris"
          (
            mkEmail
              "tonymorris"
              "gmail.com"
          )
          (
            mkLink
              "http://tmorris.net/"
              "Tony Morris"
              "HTTP"
          )
      )
      (
        mkCopyright'
          "Tony Morris"
          "2014"
          "http://en.wikipedia.org/wiki/Beerware"
      )
   ) trk

ridegpx ::
  String
ridegpx =
  "ride.gpx"
