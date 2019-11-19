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
    , start, stop, stamp, save, right, left, up, down, turn
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

atPositions(f)(ps) = pictures(foreach(nps,make))
  where
  nps = zipped([1..],ps)
  make(n,(x,y)) = translated(f(n),x,y)

atPlacements(f)(ps) = pictures(foreach(nps,make))
  where
  nps = zipped([1..],ps)
  make(n,(x,y,a)) = translated(rotated(f(n),a),x,y)

sprite pic _ = pic

sprites pics n = pics#n

render(f) = start(0,0).f.stop.pictures

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
  tpic = translated(rotated(pic,state.orientation),x,y)
  (x,y) = state.position
  
save(f)(state) = state { trace = f(state) : state.trace }

right(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((1,0),state.orientation)

left(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((-1,0),state.orientation)

up(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((0,1),state.orientation)

down(len)(state) = state { position = position' }
  where
  position' = (px+len*dx,py+len*dy)
  (px,py) = state.position
  (dx,dy) = rotatedPoint((0,-1),state.orientation)

turn(angle)(state) = state { orientation = orientation' }
  where
  orientation' = angle + state.orientation
