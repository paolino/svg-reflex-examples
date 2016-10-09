{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import           Data.FileEmbed
import           Data.Map       (Map)
import qualified Data.Map as M
import           Reflex.Dom
import Data.Text (Text,pack)
import Data.Monoid
import Data.Time.Clock
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import Lib


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
        divClass "counterDiv" $ do
            el "h3" $ text "Counter"
            counterApp


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
        ,   y <$ domEvent Touchend w
        ]

light :: MS m => DS Color ->  m ()
light color = do
    let attrs = (\c -> [("style","background-color:" <> pack (show c)),("class","light")] :: Map Text Text) <$> color
    elDynAttr "div" attrs $ ooo

buttonApp :: MS m => m ()
buttonApp = do
    btn <- fmap fst . el' "button" $ text "light"
    color <- holdDyn White $ touchMouse Red White btn
    light color


----------------------counter -------------------------------------
-- | delay an Event by the amount of time specified in its value
drivenDelay     :: MS m 
                => ES (NominalDiffTime,a) -- ^ delay time in seconds + value
                -> m (ES a)
drivenDelay e =  performEventAsync . ffor e $ \(dt,a) cb -> liftIO . void . forkIO $ do
  threadDelay . ceiling $ dt * 1000000
  cb a

-- | frequency controlled gun machine 
feedbackDelay   :: MS m 
                => BS NominalDiffTime -- ^ time interval between fires in seconds
                -> DS Bool -- ^ False is stop/stopped , True is fire/firing
                -> m (ES ())
feedbackDelay delta state  = do 
    rec    e <- drivenDelay . attach delta $ 
                leftmost [gate (current state) e, ffilter id . gate (not <$> current state) $ updated state]
    return $ () <$ e

counterApp :: MS m => m ()
counterApp = do
    up <- ((*2) <$) <$> button "slower"
    down <- ((/2) <$) <$> button "faster"
    rec     display value
            divClass "counter" $ 
                foldDyn ($) (0 :: Int) (leftmost [(+1) <$ tick, const 0 <$ t4]) >>= display
            tick <- feedbackDelay (current value) state
            value <- foldDyn ($) 0.2 $ leftmost [up,down]
            state <- holdDyn False $ leftmost [t2,t3]
            (t2,t3,t4) <- divClass "commands" $ do
                        t2 <- (False <$) <$> button "stop"
                        t3 <- (True <$) <$> button "start"
                        t4 <- button "reset"
                        return (t2,t3,t4)
    return ()
