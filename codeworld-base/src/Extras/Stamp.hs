{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | A stateful track
module Extras.Stamp(
    -- $intro
    StampTrack, render, position, trace
    , stampOnce, stampAll, stampEach, stampWith
    , start, stop, stamp, save, right, left, up, down
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

stampOnce(pic)(x,y) = translated(pic,x,y)

stampAll(pic)(pts) = pictures(foreach(pts,stampOnce(pic)))

stampEach(pics)(pts) = 
  pictures(foreach(zipped(pics,pts),\(pic,(x,y)) -> translated(pic,x,y)))

stampWith(f) = f.list.stampEach

render(f) = start(0,0).f.stop.pictures

data StampTrack a = StampTrack
  { position :: Point
  , trace :: [a]
  }

start(x,y) = StampTrack
  { position = (x,y)
  , trace = []
  }

stop(state) = state.trace.reversed

stamp(pic)(state) = state { trace = tpic : state.trace }
  where
  tpic = translated(pic,x,y)
  (x,y) = state.position
  
save(f)(state) = state { trace = f(state) : state.trace }

right(len)(state) = state { position = position' }
  where
  position' = (px+len,py)
  (px,py) = state.position

left(len)(state) = state { position = position' }
  where
  position' = (px-len,py)
  (px,py) = state.position

up(len)(state) = state { position = position' }
  where
  position' = (px,py+len)
  (px,py) = state.position

down(len)(state) = state { position = position' }
  where
  position' = (px,py-len)
  (px,py) = state.position
