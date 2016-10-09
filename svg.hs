{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}

module Main where

import           Data.FileEmbed
import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import Data.Text

ooo = return ()
svg = elDynAttrNS' (Just $ "http://www.w3.org/2000/svg")
svgA, rectA, circleA, textA :: Map Text Text 
svgA = [ ("version", "1.1")
         , ("baseProfile", "full")
         , ("width", "300px")
         , ("height", "200px")
         , ("viewBox", "0 0 300 200") ]
rectA = [("width", "100%"), ("height", "100%"), 
            ("fill", "red")]
circleA = [("cx", "150"), ("cy", "100"), ("r", "80"), 
        ("fill", "green")]
textA = [("x", "150"), ("y", "125"), ("font-size", "60"), 
        ("text-anchor", "middle"), ("fill", "white")]
main :: IO ()
-- main = mainWidgetWithCss $(embedFile "style.css") atompit
main = mainWidget $ do
    svg "svg" (pure svgA) $ do
        svg "rect" (pure rectA) $ ooo
        svg "circle" (pure circleA) $ ooo
        svg "text" (pure textA) $ text "SVG"
    return ()
