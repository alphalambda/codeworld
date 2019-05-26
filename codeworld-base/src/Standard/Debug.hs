{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}
--------------------------------------------------------------------------------
-- | The standard set of functions and variables available to all programs.
-- This version is for debugging purposes only. Look at the documentation
-- in the "Standard" module for more information.
module Standard.Debug (
      module Include
    , drawingOf, animationOf, autoSlideshow, slideshow
    -- * Redefined drawing primitives
    , solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , sector
    ) where

import Prelude as Include hiding (
    drawingOf, animationOf
    , solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , sector
    )

import qualified "codeworld-api" CodeWorld as CWA
import qualified Internal.CodeWorld as CWI
import qualified Internal.Picture as P

------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- LSU Modifications
------------------------------------------------------------------------------

solidRectangle = P.rectangle

solidCircle = P.circle

solidPolygon = P.polygon

solidClosedCurve = P.closedCurve

sector = P.arc

-- | Show a coordinate plane along with the given Picture. This is intended
-- for debugging purposes, so that you can place your objects more accurately.
-- However, it should not be used for your final product. As a reminder,
-- your picture is shown all in black.
drawingOf :: Picture -> Program
drawingOf pic = CWI.drawingOf(cfig & axes)
    where
    axes = P.coordinatePlane 
    cfig = colored(pic,RGBA(0.75,0,0,0.75))

-- | Like 'drawingOf', this function is intended for debugging animations.
animationOf :: (Number -> Picture) -> Program
animationOf draw = CWI.animationOf(draw')
    where
    draw'(t) = colored(draw(t),RGB(0.75,0,0.75)) & P.coordinatePlane

-- | Debug an slide show. Unlike the normal @slideshow@, this function
-- automatically advances slides every 2 seconds.
slideshow :: [Picture] -> Program
slideshow(slides) = autoSlideshow(slides,2)

-- | Debug an automatic slide show
autoSlideshow :: ([Picture],Number) -> Program
autoSlideshow(slides,period) = animationOf(sshow)
  where
  len = length(slides)
  sshow(t)
    | len < 1 = pictures([])
    | otherwise = slides#num
    where
    num = 1 + remainder(truncation(t/period), len)

