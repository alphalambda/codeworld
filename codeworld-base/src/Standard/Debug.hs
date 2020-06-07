{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , drawingOf, animationOf, slideshow
{-
    -- * Redefined drawing primitives
    , solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , dot
    , sector
-}
    ) where

import Prelude as Include hiding (
    drawingOf, animationOf
{-
    , solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , dot
    , sector
-}
    )

import qualified "codeworld-api" CodeWorld as CWA
import qualified Internal.CodeWorld as CWI
import qualified Internal.Picture as P
import qualified Internal.Color as C
import qualified Internal.Event as E
import Internal.Text
import Extras.Cw(graphed)

------------------------------------------------------------------------------

graphOf :: Picture -> Program
graphOf(picture) = CWI.activityOf(initial,update,draw)
  where
  initial(_) = 0
  update(t,E.TimePassing(dt)) = t + dt
  update(t,_) = t
  draw(t) = graphed(washout,1,1)
    where
    washout = P.combined([ P.colored(P.solidRectangle(20,20), C.RGBA(1,1,1,t/90))
                         , picture ])

------------------------------------------------------------------------------
-- LSU Modifications
------------------------------------------------------------------------------

{-
solidRectangle = P.rectangle

solidCircle = P.circle

solidPolygon = P.polygon

solidClosedCurve = P.closedCurve

sector = P.arc

dot(x,y) = P.combined([P.polyline(l1),P.polyline(l2)])
    where
    l1 = [(x-0.1,y+0.1),(x+0.1,y-0.1)]
    l2 = [(x-0.1,y-0.1),(x+0.1,y+0.1)]

-}

-- | Show a coordinate plane along with the given Picture. This is intended
-- for debugging purposes, so that you can place your objects more accurately.
-- However, it should not be used for your final product. As a reminder,
-- your picture will fade after 90 seconds.
drawingOf :: Picture -> Program
drawingOf = graphOf

{-
drawingOf pic = CWI.drawingOf(cfig & axes)
    where
    axes = P.coordinatePlane 
    cfig = colored(pic,RGBA(0.75,0,0,0.75))
-}

-- | Like 'drawingOf', this function is intended for debugging animations.
-- So, it will not run your animation. Instead, it expects a particular
-- timestamp, and it will show the state of your animation at that time.
animationOf :: ((Number -> Picture),Number) -> Program
animationOf(draw,instant) = drawingOf(draw(instant))

{-
animationOf draw = CWI.animationOf(draw')
    where
    draw'(t) = colored(draw(t),RGB(0.75,0,0.75)) & P.coordinatePlane
-}

-- | Debug an slide show. Unlike the normal @slideshow@, this function
-- needs the particular slide you want to show, starting at index 1.
slideshow :: ([Picture],Number) -> Program
slideshow(slides,slidenum) = drawingOf(slides#slidenum)

{-
-- | Debug an automatic slide show
autoSlideshow :: ([Picture],Number) -> Program
autoSlideshow(slides,period) = animationOf(sshow)
  where
  len = length(slides)
  sshow(t)
    | len < 1 = combined([])
    | otherwise = slides#num
    where
    num = 1 + remainder(truncated(t/period), len)

-}
