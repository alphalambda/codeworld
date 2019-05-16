{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- |A set of functions and variables that provide additional support for your
-- programs
--
module Extras.Extras
    ( stepDrawingOf, zoomableDrawingOf, clickableInterface
    , midpoint
    , rText, lText, formatting
    , approximated
    , printedPrice
    ) where

import Prelude
import qualified Data.List as L
import Internal.Text(fromCWText,toCWText)

import Extras.Op
import Extras.Cw(randomSlideshow)
import Extras.Util(list,lJustified,rJustified,printedDecimals)

import qualified "base" Prelude as P
import           "base" Prelude (map,filter,fst,snd,tail,scanl,Maybe(..))

-------------------------------------------------------------------------------
--- Top Level
-------------------------------------------------------------------------------

stepDrawingOf :: (([Number],Number) -> Picture,Number) -> Program
stepDrawingOf(draw,num) = randomSlideshow(makeslides)
    where
    makeslides(random) = first(list(slides),num)
        where
        slides(slide) = draw(random,slide)

zoomableDrawingOf :: (Picture,Picture) -> Program
zoomableDrawingOf(foreground,background) =
    interactionOf(init,update,handle,draw)
    where
    init _ = ((0,0,1),Nothing)
    update(state,_) = state
    draw((cx,cy,s),_) = scaled(translated(foreground,cx,cy),s,s) & background

    handle((state,anchor),KeyPress k) = (handleKey k state,anchor)
    handle((state,_),PointerPress(x,y)) = (state,Just(x,y))
    handle(((cx,cy,s),Just(x,y)),PointerMovement(mx,my)) = 
          ((cx+(mx-x)/s,cy+(my-y)/s,s),Just(mx,my))
    handle(((cx,cy,s),Just(x,y)),PointerRelease(mx,my)) =
          ((cx+(mx-x)/s,cy+(my-y)/s,s),Nothing)
    handle(state,_) = state

    handleKey "Up" (cx,cy,s) = (cx,cy+1,s)
    handleKey "W"  (cx,cy,s) = (cx,cy+1,s)
    handleKey "Down"  (cx,cy,s) = (cx,cy-1,s)
    handleKey "S"  (cx,cy,s) = (cx,cy-1,s)
    handleKey "Left"  (cx,cy,s) = (cx-1,cy,s)
    handleKey "A"  (cx,cy,s) = (cx-1,cy,s)
    handleKey "Right"  (cx,cy,s) = (cx+1,cy,s)
    handleKey "D"  (cx,cy,s) = (cx+1,cy,s)
    handleKey "I"  (cx,cy,s) | s >= 1/4 = (cx,cy,s+1/4)
                             | s >= 1/16 = (cx,cy,s+1/16)
                             | otherwise = (cx,cy,s+1/64)
    handleKey "O"  (cx,cy,s) | s > 1/4 = (cx,cy,s-1/4)
                             | s > 1/16 = (cx,cy,s-1/16)
                             | s > 1/64 = (cx,cy,s-1/64)
                             | otherwise = (cx,cy,1/64)
    handleKey "J"  (cx,cy,s) | s >= 1/64 = (cx,cy,2*s)
                             | otherwise = (cx,cy,s)
    handleKey "K"  (cx,cy,s) | s >= 1/32 = (cx,cy,s/2)
                             | otherwise = (cx,cy,s)
    handleKey "R" _ = firstOfPair(init[])
    handleKey _ state = state

clickableInterface :: ( [Number] -> state
                      , (Number,Number,state) -> state
                      , state -> Picture
                      ) -> Program
clickableInterface(setup,handle,draw) =
  interactionOf(setup,update,process,draw)
  where
  update(state,dt) = state
  process(state,PointerPress(x,y)) = handle(x,y,state)
  process(state,other) = state

midpoint((x0,y0),(x1,y1)) = (0.5*(x0+x1),0.5*(y0+y1))

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

-- | Simple Text formatting
--
-- To use the simple formatting described here, you need to include the
-- following line in your code:
-- 
-- > (format,spc,lit,txt,dec) = formatting
--
-- That line defines 5 functions, called formatters, that extract Numbers
-- or Text from an input data structure and produces Text padded with spaces,
-- so that the input Texts are left-justified and the input Numbers are
-- right-justified in the output. The names of the formatters are arbitrary,
-- so you can use other names for them in your code if you wish.
--
-- The formatters defined in @formatting@ are as follows:
-- 
-- > format :: [a -> Text] -> a -> Text
-- @format([formatter1,formatter2,...])(inputdata)@ applies each given
-- @fomatter@ to the input
-- data and then concatenates all the texts into a single Text.
-- The @format@ function is
-- intended to be used with the other formatters listed below.
-- 
-- > spc :: Number -> a -> Text
-- @spc(n)(inputdata)@ ignores the input data and just produces @n@ spaces
--
-- > lit :: Text -> a -> Text
-- @lit(txt)(inputdata)@ also ignores the input data and copies
-- the given @txt@ to the output.
--
-- > txt :: (a -> Text,Number) -> a -> Text
-- @txt(accessor,num)(inputdata)@ uses the given @accessor@ function to
-- extract Text from the
-- input data, and then pads the Text on the right until the length of the
-- output is @num@.
--
-- > dec :: (a -> Number,Number,Number) -> a -> Text
-- @dec(accessor,num,prec)(inputdata)@ uses the given @accessor@ function to
-- extract a Number,
-- which is then converted to Text and padded on the left until the length
-- of the output is @num@.
-- The conversion produces as many decimals as indicated by @prec@,
-- adding @0@ at the end if
-- necessary. If you want to print an integer, use @dec(accessor,num,0)@.
--
formatting :: ( [a -> Text] -> a -> Text
              , Number -> a -> Text
              , Text -> a -> Text
              , (a -> Text,Number) -> a -> Text
              , (a -> Number,Number,Number) -> a -> Text
              )
formatting = (format,spc,lit,txt,dec)
    where
    format(fmts)(d) = joined(map apply fmts)
      where
      apply(fmt) = fmt(d)

    spc(len)(d) = lJustified("",len)
    lit(str)(d) = lJustified(str,numberOfCharacters(str))
    txt(get,len)(d) = lJustified(get(d),len)
    dec(get,len,prec)(d) = rJustified(printedDecimals(get(d),prec),len)

-- | Shows left-justified text, so that the text occupies exactly the given
-- width
lText :: (Text,Number) -> Picture
lText(txt,width)
  | len < width = lText(txt <> " ",width)
  | len > width = styledLettering(cut,Monospace,Plain)
  | otherwise = styledLettering(txt,Monospace,Plain)
  where
  len = numberOfCharacters(txt)
  cut = joined(first(characters(txt),width))
 
-- | Shows right-justified text, so that the text occupies exactly the
-- given width
rText :: (Text,Number) -> Picture
rText(txt,width)
  | len < width = rText(" " <> txt,width)
  | len > width = styledLettering(cut,Monospace,Plain)
  | otherwise = styledLettering(txt,Monospace,Plain)
  where
  len = numberOfCharacters(txt)
  cut = joined(last(characters(txt),width))

-------------------------------------------------------------------------------
--- Utility Functions
-------------------------------------------------------------------------------

approximated :: (Number,Number) -> Number
approximated(x,eps) = eps * rounded (x / eps)

-- | Formats a price into an amount rounded to the closest cent, showing always two decimals
printedPrice :: Number -> Text
printedPrice(dollars) = cents
    |> abs |> printed |> addZeros |> characters
    |> addPoint |> joined |> addSign
    where
    cents = rounded(100*dollars)
    addZeros(txt) | abs(cents) < 10 = joined ["00",txt]
                  | abs(cents) < 100 = joined ["0",txt]
                  | otherwise = txt
    addPoint(chars) = first(chars,length(chars)-2) ++ ["."] ++ last(chars,2)
    addSign(txt) | signum(cents) < 0 = joined ["-",txt]
                 | otherwise = txt

-- | Either the index of the first element in list that satisfies @pred@,
-- or 0 if no element satisfies @pred@
find :: (a -> Truth,[a]) -> Number
find(pred,list) = indexOf_(1,list)
  where
  indexOf_(_,[]) = 0
  indexOf_(i,x:xs)
    | pred(x) = i
    | otherwise = indexOf_(i+1,xs)
