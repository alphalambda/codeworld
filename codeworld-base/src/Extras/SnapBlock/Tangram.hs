{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.SnapBlock.Tangram(tangramL,tangramS,tangramM,tangramD,tangramR,inventory)
where

import Prelude
import Extras.SnapBlock

tangramL = snapBlock([(-2,0),(2,0),(0,2)])
tangramS = snapBlock([(-1,0),(1,0),(0,1)])
tangramM = snapBlock([(-1,1),(1,-1),(1,1)])
tangramD = snapBlock([(-1,0)     ,(0,-1)    ,(1,0)    ,(0,1)])
tangramR = snapBlock([(-0.5,-0.5),(0.5,-1.5),(0.5,0.5),(-0.5,1.5)])

inventory = combined(
  [ translated(withLabel((tangramL,[-1,-1,1]) , solidBlock(tangramL,red)),(-7.5,7))
  , translated(minitext("2 tangramL"),(-7.5,5.5))
  , translated(withLabel((tangramS,[-1,-1,1]) , solidBlock(tangramS,red)), (8,7))
  , translated(minitext("2 tangramS"),(8,5.5))
  , translated(withLabel((tangramM,[-1,-1,1]) , solidBlock(tangramM,red)),(0,8))
  , translated(minitext("1 tangramM"),(-1,6.5))
  , translated(withLabel((tangramD,[-1,-1,1,1]) , solidBlock(tangramD,red)), (8,-6.5))
  , translated(minitext("1 tangramD"),(8,-9))
  , translated(withLabel((tangramR,[-1,-1,1,1]) , solidBlock(tangramR,red)),(-8,-6))
  , translated(minitext("1 tangramR"),(-8,-9)) ])
  where
  withLabel(a,b) = combined([labels(a), b])
  minitext(t) = dilated(lettering(t),1/2)
  red = HSL(0,0.75,0.50)
