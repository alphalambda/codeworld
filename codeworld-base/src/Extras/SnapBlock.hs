{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.SnapBlock
       ( SnapBlock, blocks, solidBlock, outlineBlock, labels
       , snapBlock, snapped, snap, anchor
       , translatedBlock, rotatedBlock, flippedBlock
       )
where

import Internal.Prelude
import Internal.Num
import Internal.Color
import Internal.Picture
import Internal.Text

newtype SnapBlock = SnapBlock [Point]

snapBlock :: [Point] -> SnapBlock
snapBlock = SnapBlock

blocks :: ([SnapBlock],Color,Color) -> Picture
blocks(bs,outer,inner) = combined(pairs)
  where
  pairs = [combined([outlineBlock(b,outer),solidBlock(b,inner)]) | b <- bs ]

solidBlock :: (SnapBlock,Color) -> Picture
solidBlock(SnapBlock(pts),color) = colored(solidPolygon(pts),color)

outlineBlock :: (SnapBlock,Color) -> Picture
outlineBlock(SnapBlock(pts),color) = colored(polygon(pts),color)

labels :: (SnapBlock,[Number]) -> Picture
labels(SnapBlock(pts),offs) = combined([ solidCircle(0.1), combined(ls) ])
  where
  ls = [translated(lbl(i),(x,y+0.5*o))
       | (x,y) <- pts
       | o <- offs
       | i <- [1..]
       ]
  lbl(i) = dilated(lettering(printed(i)),3/4)

snapped :: (Picture,SnapBlock,Number) -> Picture
snapped(pic,SnapBlock(pts),loc) = translated(pic,(tx,ty))
  where
  (tx,ty) = vertex(pts,loc)
  
snap :: (SnapBlock,SnapBlock,Number) -> SnapBlock
snap(block,SnapBlock(pts),loc) = translatedBlock(block,tx,ty)
  where
  (tx,ty) = vertex(pts,loc)
  
anchor :: (SnapBlock,Number) -> SnapBlock
anchor(block@(SnapBlock(pts)),loc) = translatedBlock(block,-tx,-ty)
  where
  (tx,ty) = vertex(pts,loc)

vertex :: ([Point],Number) -> Point
vertex(p,i) 
    | isInteger(i) = p#i
    | otherwise = vectorSum(scaledVector(p#n,1-r),scaledVector(p#n',r))
  where
  (i',r) = properFraction(i)
  n = 1 + remainder(len+i'-1,len)
  n' = 1 + remainder(len+i',len)
  len = length(p)


translatedBlock :: (SnapBlock,Number,Number) -> SnapBlock
translatedBlock(SnapBlock(pts),tx,ty) = SnapBlock([translatedPoint(p,(tx,ty)) | p <- pts])

rotatedBlock :: (SnapBlock,Number) -> SnapBlock
rotatedBlock(SnapBlock(pts),angle) = SnapBlock([rotatedPoint(p,angle) | p <- pts])

flippedBlock :: SnapBlock -> SnapBlock
flippedBlock(SnapBlock(pts)) = SnapBlock([(-x,y) | (x,y) <- pts])
