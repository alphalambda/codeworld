{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Auxiliary definitions to draw a teddy bear
module Lessons.Bear(
    dashedCircle, solidDashedCircle, thickDashedCircle,
    pink, yellow, cyan, white, teddyBrown
) where

import Prelude
import Extras.Colors

teddyBrown :: Color
teddyBrown = RGBA(0.75,0.50,0,1)

solidDashedCircle :: (Number,Color) -> Picture
solidDashedCircle(radius,color) =
 dashedCircle(radius)
 & colored(solidCircle(radius),color)

dashedCircle :: Number -> Picture
dashedCircle(radius) = thickDashedCircle(radius,0)

thickDashedCircle :: (Number,Number) -> Picture
thickDashedCircle(radius,width) 
 | radius >= 2 = combined([thickArc(i,i+5,radius,width) | i <- [0,10..350]])
 | radius >= 1 = combined([thickArc(i,i+10,radius,width) | i <- [0,20..340]])
 | otherwise = combined([thickArc(i,i+20,radius,width) | i <- [0,40..320]])
