{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions for Geometry
module Extras.Geometry
    where

import Prelude
import Extras.Op
import Extras.Util

import "base" Prelude (Maybe(..),zip)

import qualified Data.List as D

randomPoints (x:y:other) = (range x,range y) : randomPoints other
    where
    range x = 10*x-5

-------------------------------------------------------------------------------
--- Internal parameters
-------------------------------------------------------------------------------

dot_radius = 0.1
coordSize = 10.0

-------------------------------------------------------------------------------
--- Drawing
-------------------------------------------------------------------------------

label(l,(x,y)) =
  translated(scaled(lettering(l),0.75,0.75),(x+0.25,y+0.5))
  -- p +| (0.25,0)) $ P.scale textscale textscale $ P.text text

data Pt = PlusPt | EksPt | StarPt | DotPt

plus_pt = combined [ hline, vline ]
    where
    hline = polyline [(-dot_radius,0),(dot_radius,0)]
    vline = polyline [(0,-dot_radius),(0,dot_radius)]

eks_pt = combined [ lline, rline ]
    where
    lline = polyline [(-dot_radius,-dot_radius),(dot_radius,dot_radius)]
    rline = polyline [(dot_radius,-dot_radius),(-dot_radius,dot_radius)]

star_pt = combined [plus_pt, eks_pt]

dot_pt =  solidCircle(dot_radius)

drawPointAs fig (x,y) = translated(fig,(x,y))

drawPoint = drawPointAs dot_pt

drawCircle (o,p) = translated(circle(r),(x,y))
    where
    (x,y) = o
    r = dist o p

drawPointsLabels pts lbl = combined[draw pl | pl <- zip pts lbl]
    where
    draw (p,l) = combined [drawPoint p , label(l,p) ]

drawLineWith liner (p,q)
    | apart p q = liner full_line
    | otherwise = error "drawLine: singleton"
    where
    low = -(4*coordSize)
    high = 4*coordSize
    l = stdline(p,q)
    StdLine (a,b,c) = l
    full_line | abs a > abs b = [(line_getx l low,low),(line_getx l high,high)]
              | otherwise = [(low,line_gety l low),(high,line_gety l high)]

drawLine :: (Point,Point) -> Picture
drawLine = drawLineWith polyline

drawSegment :: (Point,Point) -> Picture
drawSegment(a,b) = polyline[a,b]

drawRay (p,q) = drawLineWith liner (p,q)
    where
    liner l = polyline [p,margin]
        where
        Just margin = find (beyond (p,q)) l

showPoint (x,y) = "(" <> printed(x) <> "," <> printed(y) <> ")"

showPointsLabels pts lbl = joined[showit pl | pl <- zip pts lbl]
    where
    showit (p,l) = l <> "=" <> showPoint(p) <> "  "
    
backPlane n = translated(colored(solidRectangle(n,1),RGB(1,1,1)),(0,-9.5))

-------------------------------------------------------------------------------
--- Synthetic Geometry
-------------------------------------------------------------------------------

-- apart x y tells whether x and y are distinguishable from each other
apart :: Point -> Point -> Bool
apart (x1,y1) (x2,y2) = coarse_ne x1 x2 || coarse_ne y1 y2

--badpoint (x,y) = isNaN x || isNaN y

-- Intersection between lines and circles
--
-- Lines are given by two points
-- Circles are given by the center and a point on the circumference

-- These functions may produce 0, 1 or 2 points in return

circle_circle :: (Point,Point) -> (Point,Point) -> [Point]
circle_circle = solve_circles

line_circle :: (Point,Point) -> (Point,Point) -> [Point]
line_circle (a,b) ox | apart a b = solve_line_circle (a,b) ox
                     | otherwise = error "line_circle with singleton"

line_line :: (Point,Point) -> (Point,Point) -> [Point]
line_line (a,b) (c,d) | apart a b && apart c d = solve_lines (a,b) (c,d)
                      | otherwise = error msg
    where
    msg = "line_line with singleton"
          <> if not (apart a b) then "(a,b)" else ""
          <> if not (apart c d) then "(c,d)" else ""

--- Betweeness relationship

-- Given 2 points A and B, we can divide the plane into 3 regions
-- by finding perpendicular lines to segment AB passing through A
-- and B respectively. A point X is said to be between A and B if
-- it lies in the middle region.

beyond :: (Point,Point) -> Point -> Bool
beyond = internal_beyond

outside :: (Point,Point) -> Point -> Bool
outside (a,b) x = beyond (a,b) x || beyond (b,a) x

-- between (a,b) x tells whether x is between a and b
between :: (Point,Point) -> Point -> Bool
between ab = outside ab .> not

sameside :: Point -> (Point,Point) -> Point -> Bool
sameside x (y,y') x' | not a_yy' = error "sameside"
                     | not a_xx' = True
                     | otherwise = case line_line (x,x') (y,y') of
                                    [p] -> not (between (x,x') p)
                                    [] -> True
    where a_yy' = apart y y'
          a_xx' = apart x x'

across x l = sameside x l .> not
          
-- [find p pts] returns the first point among [pts] that has property [p]
find = D.find

find_apart p points = case find (apart p) points of
    Just x -> x
    Nothing -> error ("find_apart " <> showpoint p)

showpoint (x,y) = "(" <> printed(x) <> "," <> printed(y) <> ")"

---------------------------------------------------------------------
--- Inexact numbers
---------------------------------------------------------------------

eps :: Number
eps = 0.01

coarsenum x = approximated(x,eps)
    where
    approximated(x,eps) = eps * rounded (x / eps)

coarse_gt :: Number -> Number -> Bool
coarse_gt x y = x-y > eps

coarse_lt :: Number -> Number -> Bool
coarse_lt x y = y-x > eps

coarse_ne :: Number -> Number -> Bool
coarse_ne x y = coarse_lt x y || coarse_gt x y

coarsepoint (x,y) = (coarsenum x,coarsenum y)

---------------------------------------------------------------------
--- Linear Algebra
---------------------------------------------------------------------

origin :: Point
origin = (0.0,0.0)

unitx :: Point
unitx = (1.0,0.0)

unity :: Point
unity = (0.0,1.0)

(x1,y1) +| (x2,y2) = (x1+x2,y1+y2)
(x1,y1) -| (x2,y2) = (x1-x2,y1-y2)

s *| (x,y) = (s*x,s*y)
(x,y) /| s = (x/s,y/s)
(x1,y1) .| (x2,y2) = x1*x2 + y1*y2

vec :: Point -> Point -> Point
vec (px,py) (qx,qy) = (qx-px,qy-py)

neg :: Point -> Point
neg (x,y) = (-x,-y)

ort :: Point -> Point
ort (p,q) = (-q,p)

norm :: Point -> Number
norm (x,y) = sqrt (x*x + y*y)

uvec :: Point -> Point -> Point
uvec p q = pq /| norm pq
    where pq = vec p q

projection :: (Point,Point) -> Point -> Point
projection (a,b) x = a +| ( (vec a x .| u) *| u )
    where u = uvec a b
    
-- Distance between two points
dist :: Point -> Point -> Number
dist (x1,y1) (x2,y2) = sqrt (dx*dx + dy*dy)
    where dx = x2 - x1
          dy = y2 - y1

-- (x_,y_) coordinates when axes are rotated and x_ axis passes through (a,b)
rotate_axes (a,b) (x,y) = (x_,y_)
    where x_ = (a*x+b*y)/r
          y_ = (-b*x+a*y)/r
          r = sqrt (a*a+b*b)

-- (x_,y_) coordinates when axes are translated parallel to 
-- the original with the new center at (a,b)
translate_axes :: Point -> Point -> Point
translate_axes (a,b) (x,y) = (x-a,y-b)

-- line in standard format : ax + by = c
-- should also be normalized
newtype StdLine = StdLine (Number,Number,Number)

normalize a b c | c < 0.0 = normalize (-a) (-b) (-c)
                | n > 0.0 = StdLine (a',b',c')
                | otherwise = error msg
    where
    msg = "normalize:"
           <> " a=" <> printed(a)
           <> " b=" <> printed(b)
           <> " c=" <> printed(c)
    n = sqrt (a*a + b*b)
    a' = (a/n)
    b' = (b/n)
    c' = (c/n)

stdline :: (Point,Point) -> StdLine
stdline (p,q) | apart p q = normalize a b c
              | otherwise = error "Line with singleton"
    where
    r = vec p q
    (a,b) = ort r
    c = (a,b) .| p

translate_axes_line (p,q) (StdLine (a,b,c)) = normalize a b (c-a*p-b*q)

line_gety :: StdLine -> Number -> Number
line_gety (StdLine (a,b,c)) x = (c - a*x) / b

line_getx :: StdLine -> Number -> Number
line_getx (StdLine (a,b,c)) y = (c - b*y) / a

line_isVertical :: StdLine -> Bool
line_isVertical (StdLine(a,b,c)) = not(coarse_ne b 0)

dist_point_line p l = c
    where (StdLine (_,_,c)) = translate_axes_line p l

type Circle = (Point,Point)

phase :: Point -> Number
phase (x,y) | y >= 0 = rawphase
            | otherwise = rawphase + 360.0
    where rawphase = vectorDirection(x,y)

-- Measure of angle ABC (vertex is in the middle)
angle :: Point -> Point -> Point -> Number
angle_sub p1 p2 | d < 0 = 360 + d
                | otherwise = d
    where d = p1 - p2

-- compute inner angle and give the start and stop phases
-- in counterclockwise direction
dphase a b | pos <= neg = (p1,p2)
           | otherwise = (p2,p1)
    where p1 = phase a
          p2 = phase b
          pos = angle_sub p2 p1 -- counterclockwise angle
          neg = angle_sub p1 p2 -- clockwise angle
          
-- inner angle measure
angle a o b | d >  180 = 360 - d
            | otherwise = d
    where p1 = phase (vec o a)
          p2 = phase (vec o b)
          d = abs (p2 - p1)

internal_beyond (a,b) c = angle a b c > 90

---------------------------------------------------------------------
--- Useful Constructions
---------------------------------------------------------------------

centered(pts) = foreach(pts,\p -> translatedPoint(p,(-cx,-cy)))
  where
  (cx,cy) = midpoint(pts)
  
midpoint(pts) = whileloop(input,check,next,output)
  where
  input = (pts,(0,0),0)
  check(pts,_,_) = nonEmpty(pts)
  next(pts,(sx,sy),n) = (rest(pts,1),(sx+x,sy+y),n+1) where (x,y) = pts#1
  output(_,_,0) = (0,0)
  output(_,(sx,sy),n) = (sx/n,sy/n)

mirror :: Point -> Point -> Point
mirror c x = c +| c -| x

parallel (p,q) a = (a,b)
    where
    b = a +| q -|p

perpendicular (p,q) a = (a,b)
    where
    (x,y) = q -| p
    b = a +| (-y,x)

bisector a b = (x,y)
    where
    [x,y] = circle_circle (a,b) (b,a)

---------------------------------------------------------------------
--- Triangles
---------------------------------------------------------------------

data Triangle = Triangle Point Point Point

fromPoints :: [Point] -> Triangle
fromPoints [a,b,c] = Triangle a b c

withBase :: Number -> Triangle -> Triangle
withBase base (Triangle a b c) = Triangle a' b' c
    where
        o = projection (a,b) c
        s = base / dist a b
        a' = o +| (s *| vec o a)
        b' = o +| (s *| vec o b)

withHeight :: Number -> Triangle -> Triangle
withHeight height (Triangle a b c) = Triangle a b c'
    where
        o = projection (a,b) c
        s = height / dist o c
        c' = o +| (s *| vec o c)

---------------------------------------------------------------------
--- Solver
---------------------------------------------------------------------

-------------------------------
--- Quadratic equation
-------------------------------

-- find the solutions to the standard quadratic equation: ax^2 + bx + c = 0
solve_quadratic a b c | d == 0 = [t]
                      | d > 0 = [t+h,t-h]
                      | otherwise = []
    where d = b*b - 4*a*c
          t = -b/(2*a)
          h = sqrt d / (2*a)

-------------------------------
--- Line-Line Intersections
-------------------------------

-- find the intersection of lines given by standard form: ax+by = c
solve_lines_standard (a1,b1,c1) (a2,b2,c2) | coarse_ne d 0.0 = [(x,y)]
                                           | otherwise = []
    where d = a1*b2 - a2 * b1
          x = (c1*b2 - c2*b1) / d
          y = (a1*c2 - a2*c1) / d
          
solve_lines l1 l2 = solve_lines_standard abc1 abc2
    where (StdLine abc1) = stdline l1
          (StdLine abc2) = stdline l2

-------------------------------
--- Line-Circle Intersections
-------------------------------

solve_line_centeredcircle (a,b,c) r | coarse_gt c r = []
                                    | coarse_lt c r = [proj_point +| perp, proj_point -| perp]
                                    | otherwise = [proj_point]
    where proj_point = (c*a,c*b)
          perp = d *| ort (a,b)
          d = if r*r >= c*c then sqrt (r*r - c*c)
              else error msg
          msg = "Bad sqrt in solve_line"
                <> " r=" <> printed(r)
                <> " c=" <> printed(c)

solve_line_circle l (o,p) = [translate_axes(neg o) x | x <- pts]
    where (StdLine abc) = translate_axes_line o (stdline l)
          r = dist o p
          pts = solve_line_centeredcircle abc r

-------------------------------
--- Circle-Circle Intersections
-------------------------------

radius_cond cmp r r1 r2 =
    cmp r (r1 + r2) && cmp r1 (r + r2) && cmp r2 (r + r1)

{-
radius_lt r r1 r2 =
    cmp r (r1 + r2) && cmp r1 (r + r2) && cmp r2 (r + r1)
    where cmp = coarse_lt

-}

radius_gt r r1 r2 =
    cmp r (r1 + r2) || cmp r1 (r + r2) || cmp r2 (r + r1)
    where cmp = coarse_gt

-- easy case: circle of radius r1 at (0,0) and circle of radius r2 at (r,0)
solve_circles_easy r1 r2 r | radius_cond coarse_lt r r1 r2 = [(x,y1),(x,y2)]
                           | radius_gt r r1 r2 = []
                           | coarse_gt r 0.0 = [(x,0)]
                           | otherwise = [(0,0)]
    where x = (r*r + r1*r1 - r2*r2) / 2/r
          y1 = sqrt (r1*r1 - x*x)
          y2 = -y1
                                 
-- medium case: circle r1 at (0,0) and circle r2 at (a,b)
solve_circles_medium r1 r2 (a,b) =
    case solve_circles_easy r1 r2 (sqrt (a*a+b*b)) of
        [p_] -> [rotate_axes (a,-b) p_]
        [p1_,p2_] -> [rotate_axes (a,-b) p1_, rotate_axes (a,-b) p2_]
        other -> other

-- general case
solve_circles_hard r1 (a,b) r2 (c,d) =
    case solve_circles_medium r1 r2 (c-a,d-b) of
        [(x,y)] -> [(x+a,y+b)]
        [(x1,y1),(x2,y2)] -> [(x1+a,y1+b),(x2+a,y2+b)]
        other -> other

-- solve when circles are given by center and point
solve_circles (o1,p1) (o2,p2) = solve_circles_hard r1 o1 r2 o2
    where r1 = dist o1 p1
          r2 = dist o2 p2
