{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pre-defined color constants and functions to handle color wheels
module Extras.Colors(
    colorWheel,
    black, grey, white,
    red,orange,yellow,chartreuse,green,aquamarine,cyan,azure,blue,purple,magenta,rose,
    pink,brown
    ) where

import Prelude

-- | Make a color from a color wheel with 6 color stops:
-- 'red', 'yellow', 'green', 'cyan', 'blue', 'magenta', so that
-- @colorWheel(0)@ is 'red' and @colorWheel(5)@ is 'magenta'.
colorWheel :: Number -> Color
colorWheel(x) = HSL(60*x,0.75,0.5)

-- | The color black
black :: Color
black = RGB(0,0,0)

-- | A color halfway between 'black' and 'white'
grey :: Color
grey  = RGB(0.5,0.5,0.5)

-- | The color white
white :: Color
white = RGB(1,1,1)

-- | A color from the 6-stop color wheel with index 0.
-- See 'colorWheel' for more information.
red = colorWheel(0)

-- | A color from the 6-stop color wheel with index 0.5.
-- See 'colorWheel' for more information.
orange = colorWheel(0.5)

-- | A color from the 6-stop color wheel with index 1.
-- See 'colorWheel' for more information.
yellow = colorWheel(1)

-- | A color from the 6-stop color wheel with index 1.5.
-- See 'colorWheel' for more information.
chartreuse = colorWheel(1.5)

-- | A color from the 6-stop color wheel with index 2.
-- See 'colorWheel' for more information.
green = colorWheel(2)

-- | A color from the 6-stop color wheel with index 2.5.
-- See 'colorWheel' for more information.
aquamarine = colorWheel(2.5)

-- | A color from the 6-stop color wheel with index 3.
-- See 'colorWheel' for more information.
cyan = colorWheel(3)

-- | A color from the 6-stop color wheel with index 3.5.
-- See 'colorWheel' for more information.
azure = colorWheel(3.5)

-- | A color from the 6-stop color wheel with index 4.
-- See 'colorWheel' for more information.
blue = colorWheel(4)

-- | A color from the 6-stop color wheel with index 4.5.
-- See 'colorWheel' for more information.
purple = colorWheel(4.5)

-- | A color from the 6-stop color wheel with index 5.
-- See 'colorWheel' for more information.
magenta = colorWheel(5)

-- | A color from the 6-stop color wheel with index 5.5.
-- See 'colorWheel' for more information.
rose = colorWheel(5.5)

-- | A lighter version of 'rose'
pink = lighter(rose,0.25)

-- | A duller version of 'orange'
brown = dull(orange)
