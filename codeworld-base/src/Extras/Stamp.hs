{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | A stateful track
module Extras.Stamp(
    -- $intro
    StampTrack, render, position, orientation, placement, trace
    , atPositions, atPlacements, sprite, sprites
    , start, stop, stamp, save, stampRight, stampLeft, stampUp, stampDown, turn
    ) where

import Prelude
import Extras.Cw(slideshow,randomDrawingOf)
import Extras.Op
import Extras.Util

-------------------------------------------------------------------------------
-- $intro
-- = Track API
--
-- To use the extra features in this module, you must begin your code with this
-- line:
--
-- > import Extras.Stamp
--

-------------------------------------------------------------------------------
-- Track
-------------------------------------------------------------------------------

x . f = f(x)

atPositions(f)(ps) = combined(foreach(nps,make))
  where
  nps = zipped([1..],ps)
  make(n,(x,y)) = translated(f(n),(x,y))

atPlacements(f)(ps) = combined(foreach(nps,make))
  where
  nps = zipped([1..],ps)
  make(n,(x,y,a)) = translated(rotated(f(n),a),(x,y))

sprite pic _ = pic

sprites pics n = pics#n

render(f) = start(0,0).f.stop.combined

data StampTrack a = StampTrack
  { position :: Point
  , orientation :: Number
  , trace :: [a]
  }

placement(s) = (x,y,a)
    where
    (x,y) = s.position
    a = s.orientation

start(x,y) = StampTrack
  { position = (x,y)
  , orientation = 0
  , trace = []
  }

stop(state) = state.trace.reversed

stamp(pic)(state) = state { trace = tpic : state.trace }
  where
  tpic = translated(rotated(pic,state.orientation),(x,y))
  (x,y) = state.position
  
save(f)(state) = state { trace = f(state) : state.trace }

stampRight(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((1,0),state.orientation)

stampLeft(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((-1,0),state.orientation)

stampUp(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((0,1),state.orientation)

stampDown(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((0,-1),state.orientation)

turn(angle)(state) = state { orientation = orientation' }
  where
  orientation' = angle + state.orientation
