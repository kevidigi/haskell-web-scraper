module Main where

import Text.HTML.Scalpel
import ProfScrapeLib
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Maybe
import Data.List
import Data.Foldable

title = ["Professors"]

schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]


main :: IO ()
main = do
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     toFile def "profschart.png" $ do layout_title .= "Professors per Department"
                                      layout_title_style . font_size .= 10
                                      layout_x_axis . laxis_generate .= autoIndexAxis schools
                                      plot $ fmap plotBars $ bars title (addIndexes ((map fromJust counts):[]))

