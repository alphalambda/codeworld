
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Extras.Animation
       ( Animation, Duration, Script, scriptOf
       , forAsLongAs, repeatedly, speedup, shortlived
       , slowstart, slowend, timeReversed, delayed, shifted
       , frameAt, frameAtEnd, durationOf
       , sequential, simultaneous, incremental, static
       , animated, extended, coincidental, play, (>>)
       , moving, turning, radiating, coloring, fading
       , waiting, hiding, gone, replacing
       )
where

import Internal.Prelude
import Internal.Num
import Internal.CodeWorld
import Internal.Color
import Internal.Picture
import Internal.Util

-------------------------------------------------------------------------------
-- Animation API 
-------------------------------------------------------------------------------

------------------
-- Animations
------------------

type Duration = Number

versionMajor = 0
versionMinor = 6

------------------
-- Scripts
------------------

type Script = (Duration,Animation)

-- |A delimited animation
scriptOf :: Script -> Program
scriptOf(p,move) = animationOf(move)

-- |A script that repeats for as long as the first argument runs
forAsLongAs :: (Script,Script) -> Script
forAsLongAs((p,_),(d,move)) = (p,move')
  where
  move' t = move (remainder'(t,d))

-- |A script that repeats for as long as the first argument indicates
repeatedly :: Script -> Script
repeatedly(p,move) = (0,move')
  where
  move' t = move t'
    where
    t' = remainder'(t,p)

-- |A script that runs faster (`factor > 1`) or slower (`factor < 1`) than the original.
speedup :: (Number,Script) -> Script
speedup(factor,(p,move)) = (p/factor,move')
  where
  move' t = move (t*factor)
  
-- |A script that goes backwards in time
timeReversed :: Script -> Script
timeReversed(d,move) = (d,move')
  where
  move' t | t < d = move(d-t)
          | otherwise = move(0)

-- |A script that waits some time before starting
delayed :: (Duration,Script) -> Script
delayed(wait,(d,move)) = (d+wait,move')
  where
  move' t | t < wait = blank
          | otherwise = move (t - wait)

-- |A script that starts the given duration in
shifted :: (Duration,Script) -> Script
shifted(i,(d,move)) = (d',move')
  where
  d' = if i<d then d-i else 0
  move' t = move (t+i)

-- |A script that waits at the first frame some time before starting
slowstart :: (Duration,Script) -> Script
slowstart(wait,(d,move)) = (d+wait,move')
  where
  pic = move 0
  move' t | t < wait = pic
          | otherwise = move (t - wait)

-- |A script that stays frozen at the last frame after it ends
slowend :: Script -> Script
slowend(d,move) = (d,move')
  where
  move' t | t < d = move t
          | otherwise = move d

-- |A script that goes away when it finishes
shortlived :: Script -> Script
shortlived(p,move) = (p,move')
  where
  move' t
    | t < p = move(t)
    | otherwise = blank

{-
-- |Remove duration from script
instant :: Script -> Script
instant(_,a) = (0,a)
-}

-- |A script that is extended by the given duration
continuing :: (Duration,Script) -> Script
continuing(p,script) = during(p, frameAtEnd(script))

-- |Extract the given frame from a script
frameAt :: (Number,Script) -> Picture
frameAt(t,(_,m)) = m(t)

-- |Extract the last frame from a script
frameAtEnd :: Script -> Picture
frameAtEnd(d,m) = m(d)

-- Two animations one after another
sequentially :: Script -> Script -> Script
sequentially (p1,move1) (p2,move2) = (p,move)
  where
  p = p1 + p2
  move t
    | t < p1 = move1 t
    | otherwise = move2 (t-p1)
{-
    | t < p = move2 (t-p1)
    | otherwise = move2 p2
-}

-- |Scripts that run one at a time
sequential :: [Script] -> Script
sequential [] = static(blank)
sequential [s] = s
sequential (s1:s2:ss) = sequential ((sequentially s1 s2):ss)
  
-- Two animations at the same time
simultaneously :: Script -> Script -> Script
simultaneously (p1,move1) (p2,move2) = (max(p1,p2),move)
  where
  move t = combined [move1 t , move2 t ]

-- |Scripts that run at the same time
simultaneous :: [Script] -> Script
simultaneous scripts = (p,move)
  where
  p = maximum[ p | (p,_) <- scripts ]
  move t = combined[m t | (_,m) <- scripts ]

incrementally :: Script -> Script -> Script
incrementally s1 s2 = sequentially s1 (simultaneously s1' s2)
  where
  s1' = continuing(0,s1)

-- |Like sequential, but the previous script stays on
incremental :: [Script] -> Script
incremental [] = static(blank)
incremental [s] = s
incremental (s1:s2:ss) = incremental ((incrementally s1 s2):ss)

-- |Duration of a script
durationOf :: Script -> Duration
durationOf(d,_) = d

-------------------
-- Script instances
-------------------

-- An animation of an object being translated
translating :: (Script,Number,Number) -> Script
translating((p,obj),dx,dy) = (p,move)
  where
  move t
    | t < p = translated(obj(t),(t'*dx,t'*dy))
    | otherwise = translated(obj(t),(dx,dy))
    where
    t' = t/p

-- An animation of an object being rotated
rotating :: (Script,Number) -> Script
rotating((p,obj),angle) = (p,move)
  where
  move t
    | t < p = rotated(obj(t),t'*angle)
    | otherwise = rotated(obj(t),angle)
    where
    t' = t/p

-- An animation of an object changing size
scaling :: (Script,Number,Number) -> Script
scaling((p,obj),sx,sy) = (p,move)
  where
  move t
    | t < p = scaled(obj(t),1-t'+t'*sx,1-t'+t'*sy)
    | otherwise = scaled(obj(t),sx,sy)
    where
    t' =  t/p

-- Convert a picture into a script

-- |A Picture converted into a Script
static :: Picture -> Script
static(pic) = (0,move)
  where
  move t = pic

-- |A Picture that lasts for the given duration
during :: (Duration,Picture) -> Script
during(p,pic) = (p,move)
  where
  move t = pic
  
------------------
-- Motions
------------------

type Motion = Script -> Script

-- |Apply a sequence of motions to the given picture
animated :: (Picture,[Motion]) -> Script
animated(pic,ms) = extended(static(pic),ms)

-- |Apply a sequence of motions to an existing script
extended :: (Script,[Motion]) -> Script
extended(scr,[]) = scr
extended(scr,m:ms) = extended(scr `withMotion` m,ms)

-- |Apply a motion to a currently playing script
coincidental :: (Motion,Script) -> Script
coincidental(motion,scr) = (d,move')
  where
  (d,_) = motion(scr)
  shift delta m t = m (t+delta)
  move' t = move t'
    where
    t' = remainder'(t,d)
    i = t - t'
    (_,move) = motion(shifted(i,scr))

{-
coincident :: (Script,[Motion]) -> Script
coincident(scr,[]) = scr
coincident(scr,m:ms) = coincident(m(scr),ms)
-}

withMotion :: Script -> Motion -> Script
withMotion scr smap = sequentially scr (smap(0,move'))
    where
    move' t = move (t+d)
    (d,move) = scr

-- withMotion scr smap = sequentially scr (smap(continuing(0,scr)))

-- |A motion that is a combination of a sequence of motions
play :: [Motion] -> Motion
play([]) = waiting(0)
play([m]) = m
play(m:ms) = m `andThen` play(ms)

(&>) = withMotion
infixl 5 &>

x |> f = f x
infixl 6 |>

andThen :: Motion -> Motion -> Motion
(andThen m1 m2)(s) = m1(s) &> m2

(&>>) = andThen
infixl 7 &>>

andAlso :: Motion -> Motion -> Motion
(andAlso t1 t2)(s) = t2(t1(s))

(>>) = andAlso
infixl 7 >>

-------------------
-- Motion instances
-------------------

-- | Translate by the given units to the right and upwards
moving :: (Duration,Number,Number) -> Motion
moving (p,dx,dy)(_,a) = translating((p,a),dx,dy)

rotating_ :: (Duration,Number) -> Motion
rotating_(p,angle)(_,a) = rotating((p,a),angle)

scaling_ :: (Duration,Number,Number) -> Motion
scaling_(p,sx,sy)(_,a) = scaling((p,a),sx,sy)

-- | Ramp color up from transparent to given color
coloring :: (Duration,Color) -> Motion
coloring(d,RGBA(r,g,b,a))(_,m) = (d,m')
  where
  m'(t) = colored(m(t),color(t))
  color(t) | t < d = RGBA(t'*r,t'*g,t'*b,t'*a)
           | otherwise = RGBA(r,g,b,a)
    where
    t' = t/d

-- | Ramp color down from given color to transparent
fading :: (Duration,Color) -> Motion
fading(d,RGBA(r,g,b,a))(_,m) = (d,m')
  where
  m'(t) = colored(m(t),color(t))
  color(t) | t < d = RGBA(t'*r,t'*g,t'*b,max(0.001,t'*a))
           | otherwise = RGBA(0,0,0,0.001)
    where
    t' = 1 - t/d

{-
-- Rotate by given angle around a pivot (cx,cy)
turning :: (Duration,Number,Number,Number) -> Motion
turning(p,cx,cy,angle)(_,a) = (p,a3)
  where
  (_,a1) = translating((0,a),(-cx,-cy)) -- 
  (_,a2) = rotating((p,a1),angle)     -- s2 = rotating_(p,angle)(s1)
  (_,a3) = translating((0,a2),(cx,cy))  -- s3 = moving(0,cx,cy)(s2)
-}

-- |Rotate around a pivot `(cx,cy)` by the given angle
turning :: (Duration,Number,Number,Number) -> Motion
turning(p,cx,cy,angle) =
  moving(0,-cx,-cy)
  >> rotating_(p,angle)
  >> moving(0,cx,cy)
  >> waiting(p)
  where
  
-- |Scale with center of similarity at `(cx,cy)` and factors `(sx,sy)`
radiating :: (Duration,Number,Number,Number,Number) -> Motion
radiating(p,cx,cy,sx,sy) =
  moving(0,-cx,-cy)
  >> scaling_(p,sx,sy)
  >> moving(0,cx,cy)
  >> waiting(p)

-- Utility functions

-- |Make a script last for the given duration
waiting :: Duration -> Motion
waiting(p)(_,a) = (p,a)

-- |Do not show the script for the given duration
hiding :: Duration -> Motion
hiding(p)(_,a) = (p,move)
  where
  move t
    | t < p = blank
    | otherwise = a(t)

-- |Replace the script with a blank
gone :: Motion
gone _ = static(blank)

-- |Replace the script with the given one
replacing :: Script -> Motion
replacing(s) _ = s

remainder'(t,d) | t == 0 = 0
                | t' == 0 = d
                | otherwise = t'
  where
  t' = remainder(t,d)

