{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where

import Data.List
import Text.HTML.Scalpel

numProfessors :: String -> IO (Maybe Int)
numProfessors x = do
  res <- scrapeURL (generateURL x) scrapeGroups
  return $ length . profsOnly . concat <$> concat <$> res

generateURL :: String -> String
generateURL s = "https://www.gla.ac.uk/schools/" ++ s ++ "/staff"

scrapeGroups :: Scraper String [[[String]]]
scrapeGroups =
  chroots ("div" @: ["id" @= "tabs"]) $ noFakes

noFakes :: Scraper String [[String]]
noFakes = 
  chroots ("ul" @: [notP ("id" @= "honorary-visitinglist")]) $ scrapeNames

scrapeNames :: Scraper String [String]
scrapeNames = do
  profs <- texts "a"
  return profs

profsOnly :: [String] -> [String]
profsOnly = filter (isInfixOf "Prof")
