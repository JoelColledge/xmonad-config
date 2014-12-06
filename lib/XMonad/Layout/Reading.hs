{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.Reading ( Reading(..) ) where

import XMonad.Core
import XMonad.Layout(Resize(..), IncMasterN(..), splitVertically)

import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import Control.Monad

-- | Single centered 'master' column. Supports 'Shrink' and 'Expand'.
data Reading a = Reading {
                     tallRatioIncrement :: !Rational   -- ^ Percent of screen to increment by when resizing panes (default: 3/100)
                   , tallRatio :: !Rational            -- ^ Default proportion of screen occupied by master pane (default: 1/2)
                   }
                deriving (Show, Read)
                        -- TODO should be capped [0..1] ..

-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass Reading a where
    pureLayout (Reading _ frac) r s = [(W.focus s, centerHorizontallyBy frac r)]

    pureMessage (Reading delta frac) m =
            msum [fmap resize     (fromMessage m)]

      where resize Shrink             = Reading delta (max 0 $ frac-delta)
            resize Expand             = Reading delta (min 1 $ frac+delta)

    description _ = "Reading"

-- Calculate horizontally centered rectangle, using a rational to specify the width ratio
centerHorizontallyBy :: RealFrac r => r -> Rectangle -> Rectangle
centerHorizontallyBy f (Rectangle sx sy sw sh) =
    Rectangle (sx + (fromIntegral sw - w) `div` 2) sy (fromIntegral w) sh
  where w = floor $ fromIntegral sw * f
