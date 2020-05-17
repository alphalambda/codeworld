{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.Game
where

import Prelude
import Extras.Do

-------------------------------------------------------------------------------
-- Game Loop
-------------------------------------------------------------------------------

x .# f = f(x)

infixl 8 .#

data Context a = Context
  { frames :: Number
  , time   :: Number
  , today  :: (Number,Number,Number)
  , now    :: (Number,Number,Number)
  , random :: [Number]
  , randomPool :: [Number]
  , state  :: a
  }

gameLoop :: ( [Number] -> state
            , (Context state,Number) -> state
            , (Context state,Event ) -> state
            , Context state -> Picture
            ) -> Program
gameLoop(init,update,handle,draw) = do
  now   <- Extras.Do.now
  today <- Extras.Do.today
  activityOf(init'(today,now),update',draw')
    where
    init'(today,now)(r:rs) = Context
      { frames = 0
      , time = 0
      , today = today
      , now = now
      , random = firstPool
      , randomPool = rs
      , state = init(firstPool)
      }
        where
        firstPool = randomNumbers(r)

    update'(ctx@Context{..},event) =
        case event of
        TimePassing(dt) -> ctx
                           { frames = frames + 1
                           , time = time + dt
                           , random = randomNumbers(randomPool#1)
                           , randomPool = rest(randomPool,1)
                           , state = update(ctx,dt)
                           }
        _ -> ctx { state = handle(ctx,event) }

    draw' = draw

