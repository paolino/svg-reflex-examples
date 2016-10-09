{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}
{-# LANGUAGE ConstraintKinds       #-}

module Main where

import           Data.FileEmbed
import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import Data.Text (Text,pack)
import Data.Monoid

type SG = SpiderTimeline Global
type DS = Dynamic SG
type ES = Event SG
type MS = MonadWidget SG

ooo = return ()


main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ do
    el "body" $ do
        divClass "svgDiv" $ do
            el "h3" $ text "SVG"
            svgApp
        divClass "buttonDiv" $ do
            el "h3" $ text "Button"
            buttonApp


--------------------- svg --------------------------------
svgApp :: MS m => m ()
svgApp = svg "svg" (pure svgA) $ do
                svg "rect" (pure rectA) $ ooo
                svg "circle" (pure circleA) $ ooo
                svg "text" (pure textA) $ text "SVG"

svg :: MS m => Text -> DS (Map Text Text) -> m () -> m ()
svg n a r = elDynAttrNS' (Just $ "http://www.w3.org/2000/svg") n a r >> ooo

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

-------------------- button ---------------------
data Color = White | Red deriving Show

touchMouse x y w = leftmost 
        [   x <$ domEvent Mousedown w
        ,   y <$ domEvent Mouseup w
        ,   x <$ domEvent Touchstart w
        ,   y <$ domEvent Touchend w]

light :: MS m => DS Color ->  m ()
light color = do
    let attrs = fmap (\c -> [("style","background-color:" <> pack (show c)),("class","light")] :: Map Text Text) color

    elDynAttr "div" attrs $ ooo

buttonApp :: MS m => m ()
buttonApp = do
    btn <- fmap fst . el' "button" $ text "light"
    color <- holdDyn White $ touchMouse Red White btn
    light color

