{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
-- |A set of functions and variables that provide additional support for your
-- programs
--
module Extras( slideshow, autoSlideshow, randomSlideshow, randomAutoSlideshow
             , randomAnimationOf, randomDrawingOf, stepDrawingOf, zoomableDrawingOf, clickableInterface
             , between, beyond, saw
             , closedCurvePoints, openCurvePoints, midpoint, grid
             , rgb, withAlpha, rText, lText, rJustified, lJustified, formatting, textHash, precedes
             , approximated, messages
             , printedNumbers, printedDecimals, printedPrice, printedPair, overlays, underlays
             , alphabeticalSortedOn, numericalSortedOn, alphabeticalGroupBy, numericalGroupBy
             , map, filter, (<|), (|>), (<$>), (<?>), (<|>), (<&>)
             , run, repeat, repeats, repeatWhile, repeatsWhile, foreach, forloop, iterated
             , prepend, append, list, listn, pairs, unpairs, inGroupsOf, unzipped, zipped
             , cumsum
             , indexOf, find
             ) where

import Prelude
import qualified Data.List as L
import Internal.Text(fromCWText,toCWText)
import Extras.Op
import qualified "base" Prelude as P
import           "base" Prelude (map,filter,fst,snd,tail,scanl,Maybe(..))

-------------------------------------------------------------------------------
--- Top Level
-------------------------------------------------------------------------------

data SS = SS
  { time :: Number
  , tlast :: Number
  , current :: Number
  , random :: [Number]
  , slides :: [Picture]
  }
  
slideshow :: [Picture] -> Program
slideshow(slides) = randomSlideshow(makeslides)
    where
    makeslides _ = slides

randomSlideshow :: ([Number] -> [Picture]) -> Program
randomSlideshow(mkslides) = interactionOf(initial,update,handle,render)
    where
    initial (r:rs) = SS { time = 0, tlast = 0, current = 1, random = rs
                        , slides = mkslides(randomNumbers(r))
                        }
    update(ss@SS{..},dt) = ss { time = time + dt }
    
    render(SS{..})
      | empty(slides) = pictures([])
      | otherwise     = showSlide & slides#current
      where
      showSlide
          | time - tlast > 2 = blank
          | otherwise = translated(mark,-9.5,-9.5)
                
      mark = scaled(lettering(printed(current)),0.5,0.5)
           & colored(solidRectangle(1,1),RGB(0.9,0.9,0.9))
           
    handle(ss,event) = mayHandleEvent(ss)
      where
      handleNav(c,s) = case event of
          KeyPress "R" -> 1+length(s)
          KeyPress "N" -> c+1
          KeyPress " " ->  c+1
          KeyPress "Enter" -> c+1
          KeyPress "PageDown" -> c+1
          KeyPress "P" -> c-1
          KeyPress "PageUp" -> c-1
          KeyPress "Backspace" -> c-1
          PointerPress _ -> c+1
          other -> c
        
      handlePan(c,s) = case event of
          KeyPress "Left" -> over c moveleft s
          KeyPress "Right" -> over c moveright s
          KeyPress "Up" -> over c moveup s
          KeyPress "Down" -> over c movedown s
          KeyPress "A" -> over c moveleft s
          KeyPress "D" -> over c moveright s
          KeyPress "W" -> over c moveup s
          KeyPress "S" -> over c movedown s
          --KeyPress key -> (0,[messages [key]])
          other -> s
          where
          moveleft(p)  = translated(p,-1, 0)
          moveright(p) = translated(p, 1, 0)
          moveup(p)    = translated(p, 0, 1)
          movedown(p)  = translated(p, 0,-1)
          over n f slides = [ if i == n then f s else s
                            | s <- slides
                            | i <- [1..]
                            ]

      mayHandleEvent(ss) = wrap(handleEvent(ss))
          
      handleEvent(ss@SS{..}) = ss
          { current = nextCurrent
          , tlast = time
          , slides = handlePan(current,slides)
          }
          where
          nextCurrent = handleNav(current,slides)
          
      remake(ss@SS{..}) = ss
          { random = rs
          , slides = mkslides(randomNumbers(r))
          }
          where
          r:rs = random

      realign(ss,newCurrent) = ss {current = newCurrent}
      
      wrap(ss@SS{..})
          | current > nslides = wrap(realign(remake(ss),current-nslides))
          | current < 1 = wrap(realign(remake(ss),current+nslides))
          | otherwise = ss
          where
          nslides = length(slides)

autoSlideshow :: ([Picture], Number) -> Program
autoSlideshow(slides,period) = animationOf(sshow)
  where
  len = length(slides)
  sshow(t)
    | len < 1 = pictures([])
    | otherwise = slides#num
    where
    num = 1 + remainder(truncation(t/period), len)

randomAutoSlideshow :: ([Number] -> [Picture], Number) -> Program
randomAutoSlideshow(mkslides,period) = simulationOf(initial,update,render)
  where
    initial (r:rs) = SS { time = 0, tlast = 0, current = 1, random = rs
                        , slides = mkslides(randomNumbers(r))
                        }

    update(ss,dt) = update_wrap(update_current(update_time(ss,dt)))
    
    update_time(ss@SS{..},dt) = ss { time = time + dt }

    update_current(ss@SS{..})
        | time - tlast > period = ss { tlast = tlast + period
                                     , current = current + 1
                                     }
        | otherwise = ss

    update_wrap(ss@SS{..})
        | current > length(slides) = ss { current = 1
                                        , random = rs
                                        , slides = mkslides(randomNumbers(r))
                                        }
        | otherwise = ss
            where
            r:rs = random
            
    render(SS{..}) = slides#current

randomAnimationOf :: (([Number],Number) -> Picture) -> Program
randomAnimationOf(movie) = interactionOf(initial,update,handle,draw)
    where
    initial (seed:rs) = (rs,0,seed)

    update((rs,t,seed),dt) = (rs,t+dt,seed)

    handle((rs,t,seed),PointerPress(_)) = (newrs,t,newseed)
        where
        newseed:newrs = randomNumbers(seed)
    handle(model,_) = model

    draw(rs,t,seed) = movie(rs,t)

randomDrawingOf :: ([Number] -> Picture) -> Program
randomDrawingOf(makeDrawing) = interactionOf(initial,update,handle,draw)
    where
    initial rs = rs
    update(model,_) = model
    handle(r:rs,PointerPress(_)) = rs
    handle(model,_) = model
    draw(r:_) = makeDrawing(randomNumbers(r))

stepDrawingOf :: (([Number],Number) -> Picture,Number) -> Program
stepDrawingOf(draw,num) = randomSlideshow(makeslides)
    where
    makeslides(random) = first(list(slides),num)
        where
        slides(slide) = draw(random,slide)

zoomableDrawingOf :: (Picture,Picture) -> Program
zoomableDrawingOf(foreground,background) = interactionOf(init,update,handle,draw)
    where
    init _ = ((0,0,1),Nothing)
    update(state,_) = state
    draw((cx,cy,s),_) = scaled(translated(foreground,cx,cy),s,s) & background

    handle((state,anchor),KeyPress k) = (handleKey k state,anchor)
    handle((state,_),PointerPress(x,y)) = (state,Just(x,y))
    handle(((cx,cy,s),Just(x,y)),PointerMovement(mx,my)) = ((cx+(mx-x)/s,cy+(my-y)/s,s),Just(mx,my))
    handle(((cx,cy,s),Just(x,y)),PointerRelease(mx,my)) = ((cx+(mx-x)/s,cy+(my-y)/s,s),Nothing)
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

clickableInterface :: ([Number] -> state, (Number,Number,state) -> state, state -> Picture) -> Program
clickableInterface(setup,handle,draw) = interactionOf(setup,update,process,draw)
  where
  update(state,dt) = state
  process(state,PointerPress(x,y)) = handle(x,y,state)
  process(state,other) = state

-------------------------------------------------------------------------------
-- Animation Helpers
-------------------------------------------------------------------------------

-- | This function is used in animations. The expression @between(t,start,stop,drawing)@ will show
-- the given @drawing@ when the time @t@ is between @start@ and @stop@
between :: (Number,Number,Number,Picture) -> Picture
between(t,start,stop,drawing) =
  if t < start then blank
  else if t < stop then drawing
  else blank

-- | This function is used in animations. The expression @beyond(t,start,drawing)@ will
-- show the given @drawing@ when the time @t@ is beyond the @start@ time
beyond :: (Number,Number,Picture) -> Picture
beyond(t,start,drawing) =
  if t < start then blank
  else drawing

-- | This function is used in animations. The expression @saw(t,p)@ is @0@
-- when @t=0@, increases up to 1 when @t=p/2@, and then decreases back to 0 when @t=p@.
-- This increasing and decreasing when @t@ goes from @0@ to @p@ is called an oscillation
-- of period @p@. The oscillations will keep repeating, so that the function is @0@ when
-- @t@ is @0,p,2p,3p,4p,5p,...@ and it is 1 when @t@ is @p/2@,@3p/2@,@5p/2@,@7p/2@,...
saw :: (Number,Number) -> Number
saw(t,p) = 1 - abs(2*abs(remainder(t,p))/p - 1)

------------------------------------------------------------------------------
-- Bezier interpolation 
------------------------------------------------------------------------------

-- | @openCurvePoints(controls,distance)@ is a list of points that approximate
-- a curve passing through the given @controls@. A variable number of points
-- is generated in such a way that the distance between them is approximately
-- the given @distance@.
openCurvePoints :: ([Point],Number) -> [Point]

-- | This function is similar to @curvePoints@, but the points approximate
-- a closed curve passing through the given controls.
closedCurvePoints :: ([Point],Number) -> [Point]

(openCurvePoints,closedCurvePoints) = (ocp,ccp)
  where
  -- 1D Linear interpolation
  lerp1(a,b,t) = (1-t)*a + t*b

  -- 2D Linear interpolation
  lerp2((x0,y0),(x1,y1),t) = (lerp1(x0,x1,t),lerp1(y0,y1,t))

  -- 2D Quadratic interpolation
  qerp2(p0,p1,p2,t) = lerp2(lerp2(p0,p1,t),lerp2(p1,p2,t),t)

  -- 2D Cubic interpolation
  cerp2(p0,p1,p2,p3,t) = lerp2(qerp2(p0,p1,p2,t),qerp2(p1,p2,p3,t),t)

  -- initial interpolation (p1,p2)
  bezierFirst(p1,p2,p3,grain) =
    let
    c = vectorSum(p2,scaledVector(vectorDifference(p1,p3),r/2))
    r = d12 / (d12 + d23)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    l = 1 - grain / d12
    in
    [ qerp2(p1,c,p2,t) | t <- [0,grain..l] ]

  -- final interpolation (p2,p3)
  bezierLast(p1,p2,p3,grain) =
    let
    c = vectorSum(p2,scaledVector(vectorDifference(p3,p1),r/2))
    r = d23 / (d12 + d23)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    l = 1 - grain / d23
    in
    [ qerp2(p2,c,p3,t) | t <- [0,grain..l] ]

  -- middle interpolation (p2,p3)
  bezierMiddle(p1,p2,p3,p4,grain) =
    let
    c1 = vectorSum(p2,scaledVector(vectorDifference(p3,p1),r1/2))
    c2 = vectorSum(p3,scaledVector(vectorDifference(p2,p4),r2/2))
    r1 = d23 / (d12 + d23)
    r2 = d23 / (d23 + d34)
    d12 = dist(p1,p2)
    d23 = dist(p2,p3)
    d34 = dist(p3,p4)
    l = 1 - grain / d23
    in
    [ cerp2(p2,c1,c2,p3,t) | t <- [0,grain..l] ]


  ccp([],_) = []
  ccp([p1],_) = []
  ccp([p1,p2],_) = [p1,p2]
  ccp(ps,grain) = go(last(ps,1) ++ ps ++ first(ps,2))
    where
    go(p1:p2:p3:p4:more) = bezierMiddle(p1,p2,p3,p4,grain) ++ go(p2:p3:p4:more)
    go(_) = []

  ocp([],_) = []
  ocp([p1],_) = []
  ocp([p1,p2],_) = [p1,p2]
  ocp(p1:p2:p3:more,grain) = bezierFirst(p1,p2,p3,grain) ++ go(p1:p2:p3:more)
    where
    go(p1:p2:p3:p4:more) = bezierMiddle(p1,p2,p3,p4,grain) ++ go(p2:p3:p4:more)
    go([p1,p2,p3]) = bezierLast(p1,p2,p3,grain)
    go(_) = []

  dist(p,q) = vectorLength(vectorDifference(p,q))


-------------------------------------------------------------------------------
--- Points
-------------------------------------------------------------------------------

{-
rotatedPoint :: (Point,Number) -> Point
rotatedPoint = rotatedVector

scaledPoint :: (Point,Number,Number) -> Point
scaledPoint((x,y),sx,sy) = (x*sx,y*sy)

translatedPoint :: (Point,Number,Number) -> Point
translatedPoint((x,y),tx,ty) = (x+tx,y+ty)
-}

midpoint((x0,y0),(x1,y1)) = (0.5*(x0+x1),0.5*(y0+y1))

-------------------------------------------------------------------------------
-- Colors
-------------------------------------------------------------------------------

rgb :: (Number,Number,Number) -> Color
rgb(r,g,b) = RGB(r/256,g/256,b/256)

withAlpha :: (Color,Number) -> Color
withAlpha(RGBA(r,g,b,_),a) = RGBA(r,g,b,a)

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

-- | Simple Text formatting
--
-- To use the simple formatting described here, you need to include the following
-- line in your code:
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
-- @format([formatter1,formatter2,...])(inputdata)@ applies each given @fomatter@ to the input
-- data and then concatenates all the texts into a single Text. The @format@ function is
-- intended to be used with the other formatters listed below.
-- 
-- > spc :: Number -> a -> Text
-- @spc(n)(inputdata)@ ignores the input data and just produces @n@ spaces
--
-- > lit :: Text -> a -> Text
-- @lit(txt)(inputdata)@ also ignores the input data and copies the given @txt@ to the output.
--
-- > txt :: (a -> Text,Number) -> a -> Text
-- @txt(accessor,num)(inputdata)@ uses the given @accessor@ function to extract Text from the
-- input data, and then pads the Text on the right until the length of the output is @num@.
--
-- > dec :: (a -> Number,Number,Number) -> a -> Text
-- @dec(accessor,num,prec)(inputdata)@ uses the given @accessor@ function to extract a Number,
-- which is then converted to Text and padded on the left until the length of the output is @num@.
-- The conversion produces as many decimals as indicated by @prec@, adding @0@ at the end if
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

-- | Shows left-justified text, so that the text occupies exactly the given width
lText :: (Text,Number) -> Picture
lText(txt,width)
  | len < width = lText(txt <> " ",width)
  | len > width = styledLettering(cut,Monospace,Plain)
  | otherwise = styledLettering(txt,Monospace,Plain)
  where
  len = numberOfCharacters(txt)
  cut = joined(first(characters(txt),width))
 
-- | Justifies text to the right, and adds padding on the left, so that the
-- text occupies exactly the given width
lJustified :: (Text,Number) -> Text
lJustified(txt,width)
  | len < width = lJustified(txt <> " ",width)
  | len > width = cut
  | otherwise = joined(last(characters(txt),width))
  where
  len = numberOfCharacters(txt)
  cut = joined(first(characters(txt),width))
  
-- | Shows right-justified text, so that the text occupies exactly the given width
rText :: (Text,Number) -> Picture
rText(txt,width)
  | len < width = rText(" " <> txt,width)
  | len > width = styledLettering(cut,Monospace,Plain)
  | otherwise = styledLettering(txt,Monospace,Plain)
  where
  len = numberOfCharacters(txt)
  cut = joined(last(characters(txt),width))

-- | Justifies text to the right, and adds padding on the left, so that the
-- text occupies exactly the given width
rJustified :: (Text,Number) -> Text
rJustified(txt,width)
  | len < width = rJustified(" " <> txt,width)
  | len > width = cut
  | otherwise = joined(last(characters(txt),width))
  where
  len = numberOfCharacters(txt)
  cut = joined(last(characters(txt),width))
  
-- | Creates a numeric hash out of a text
textHash :: Text -> Number
textHash(s) = go h0 (characters s)
  where
  a = 33
  h0 = 5381
  p = 1001
  go h [] = h
  go h (r:rs) = 
    let h' = remainder(h*a+lookup(r),p)
    in go h' rs

  lookup c = go' 0 chars
    where
    go' i [] = i
    go' i (r:rs) | c == r = i
                 | otherwise = go' (i+1) rs

  chars = characters(lower<>upper<>other)
    where
    lower = "abcdefghijklmnopqrstuvxyz"
    upper = uppercase(lower)
    other = " .,'!@#$%^&*()-=_+|/<>\\"


-------------------------------------------------------------------------------
--- Grid
-------------------------------------------------------------------------------

-- | A @grid(pic,rows,columns)@ is a grid with the given number of @rows@ and
-- @columns@, where the rows are numbered top to bottom (top row is row 1)
-- and the columns are numbered left to right (leftmost column is column 1).
-- The user needs to specify what to draw at each cell in the grid. The given
-- function @pic@ should specify a 20 by 20 picture for each row and column,
-- 
-- > pic(row,col) = fullSizePicture
--
-- Each full size picture will be scaled to fit within the cell.
-- Example:
--
-- > picture = grid(pic,10,10)
-- >     where
-- >     pic(row,col) = dilated(lettering(coord),5)
-- >                  & rectangle(19,19)
-- >         where
-- >         coord = joined(["(",printed(row),",",printed(col),")"])
--
-- The example above will show the row index and the column index for each cell
grid :: ((Number,Number) -> Picture,Number,Number) -> Picture
grid(pic,rows,cols) = translated(pictures(rpic),-10,10)
  where
  w = 20/cols
  h = 20/rows
  transform(row,col) = translated(base,w*(col-1/2),-h*(row-1/2))
    where
    base = scaled(pic(row,col),1/cols,1/rows)
  rpic = [ transform(row,col) | row <- [1..rows], col <- [1..cols] ]


-------------------------------------------------------------------------------
--- Utility Functions
-------------------------------------------------------------------------------

approximated :: (Number,Number) -> Number
approximated(x,eps) = eps * rounded (x / eps)

-- | Creates a list of cumulative sums
--
-- > cumsum([1,2,3,4]) -- > [1,1+2,1+2+3,1+2+3+4]
--
cumsum :: [Number] -> [Number]
cumsum = tail .< scanl (+) 0

-- | Formats a Number, so that it always shows the given number of decimals
printedDecimals :: (Number,Number) -> Text
printedDecimals(number,prec)
    | number < 0 = "-" <> printedDecimals(-number,prec)
    | prec <= 0 = printed(rounded(number))
    | otherwise = joined(go(characters(printed(rounded(number*10^prec)))))
    where
    go(n) | len > prec = first(n,len-prec) ++ ("." : last(n,prec))
          | otherwise = "0" : "." : fill(prec-len,n)
          where
          len = length(n)
          fill(0,txt) = txt
          fill(num,txt) = "0" : fill(num-1,txt)
                                                                       
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

messages :: [Text] -> Picture
messages(lines) = pictures([showline(i) | i <- [1..n]])
    where
    n = length(lines)
    showline(i) = translated(scaled(fmt(lines#i),0.5,0.5),0,10.25-0.5*i)
    -- Output should be 40 rows and 66 columns
    fmt(txt) = styledLettering(lJustified(txt,66),Monospace,Italic)

printedNumbers :: [Number] -> Text
printedNumbers(list) = "[" <> printedRawNumbers(list) <> "]"
  where
  printedRawNumbers[] = ""
  printedRawNumbers[n] = printed(n)
  printedRawNumbers(n:ns) = printed(n) <> ", " <> printedRawNumbers(ns)

printedPair :: (Number,Number) -> Text
printedPair(x,y) = "(" <> printed(x) <> "," <> printed(y) <> ")"

-- | @overlays(fig,n) = fig(1) & fig(2) & ... & fig(n)@
overlays :: ((Number -> Picture),Number) -> Picture
overlays(f,n) = overlays'(f,max(0,truncation(n)))
    where
    overlays'(f,0) = blank
    overlays'(f,n) = overlays'(f,n-1) & f(n)

-- | @underlays(fig,n) = fig(n) & fig(n-1) & ... & fig(1)@
underlays :: ((Number -> Picture),Number) -> Picture
underlays(f,n) = underlays'(f,max(0,truncation(n)))
    where
    underlays'(f,0) = blank
    underlays'(f,n) = f(n) & underlays'(f,n-1)

-- | @numericalSortedOn(value)(list)@ is sorted in the following way:
-- if @value(element1) < value(element2)@ then @element1@ will apear before
-- @element2@
numericalSortedOn :: (a -> Number) -> [a] -> [a]
numericalSortedOn = L.sortOn

-- | @alphabeticalSortedOn(value)(list)@ is sorted in the following way:
-- if @value(element1)@ precedes @value(element2)@ alphabetically
-- then @element1@ will apear before @element2@
alphabeticalSortedOn :: (a -> Text) -> [a] -> [a]
alphabeticalSortedOn get = L.sortOn (fromCWText .< get)

-- |@numericalGroupBy(value)(list)@ breaks up the given @list@ into sublists
-- of equal @value@. It creates a list @(v1,elements1),(v2,elements2)...@ so that all the
-- elements in @elements1@ have value @v1@, all the elements in @elements2@
-- have value @v2@ and so on. Note that @value@ is a function, so the
-- value of an element @x@ is given by @value(x)@.
numericalGroupBy :: (a -> Number) -> [a] -> [(Number,[a])]
numericalGroupBy get = groupBy .< numericalSortedOn fst .< map build
    where
    build x = (get x,x)

-- |@alphabeticalGroupBy(value)(list)@ breaks up the given @list@ into sublists
-- of equal @value@. It creates a list @(v1,elements1),(v2,elements2)...@ so that all the
-- elements in @elements1@ have value @v1@, all the elements in @elements2@
-- have value @v2@ and so on. Note that @value@ is a function, so the
-- value of an element @x@ is given by @value(x)@.
alphabeticalGroupBy :: (a -> Text) -> [a] -> [(Text,[a])]
alphabeticalGroupBy get = groupBy .< alphabeticalSortedOn fst .< map build
    where
    build x = (get x,x)

-- |@precedes(text1,text2)@ is @True@ whenever @text1@ precedes @text2@ alphabetically. It is @False@
-- otherwise. Note that when the two texts are the same, the result of @precedes@ is @False@.
precedes :: (Text,Text) -> Truth
precedes(a,b) = fromCWText a P.< fromCWText b

-- Not exported
groupBy :: [(a,b)] -> [(a,[b])]
groupBy [] = []
groupBy ((x,d):xds) = (x,d:ds) : groupBy xds_ne
    where
    (xds_eq,xds_ne) = L.span ((x ==) .< fst) xds
    ds = map snd xds_eq

-------------------------------------------------------------------------------
--- Language Extensions
-------------------------------------------------------------------------------

-- | Run a sequence of transformations in order on the given initial state.
-- For example, the expression @run([f1,f2,f3])(x)@
-- is the same as @f3(f2(f1(x)))@
run :: [a -> a] -> a -> a
run([])(x) = x
run(f:fs)(x) = run(fs)(f(x))

-- | Repeat a transformation the given number of times.
-- For example, the expression @repeat(3,f)(x)@ is the same as @f(f(f(x)))@
repeat :: (Number,a -> a) -> a -> a
repeat(0,f)(x) = x
repeat(n,f)(x) = repeat(n-1,f)(f(x))

-- | Repeat a sequence of transformations a given number of times
repeats :: (Number,[a -> a]) -> a -> a
repeats(n,fs) = repeat(n,run(fs))

-- | Keep repeating a transformation while the given predicate is True
repeatWhile :: (a -> Truth,a -> a) -> a -> a
repeatWhile(cond,f)(x) = if cond x then repeatWhile(cond,f)(f x) else x

-- | Keep repeating a sequence of transformations while the given predicate is True
-- The output is the first value that does not satisfy the predicate.
repeatsWhile :: (a -> Truth,[a -> a]) -> a -> a
repeatsWhile(cond,fs)(x) = loop (repeating fs) x
    where
    loop [] x = x
    loop (f:fs) x = if cond x then loop fs (f x) else x

-- | Constructs a list by applying a function
-- to all the elements of a given list
foreach :: ([a],a -> b) -> [b]
foreach(l,f) = map f l

-- | @forloop(init,check,next,action)@ produces a list of outputs, where each
-- output is given by repeatedly applying @action(state)@. The loop starts
-- with the given initial @state@, and new states are generated from it
-- by the expression @next(state)@. Generation of new states continues
-- for as long as @check(state)@ is True.
forloop :: (a,a -> Truth,a -> a,a -> b) -> [b]
forloop(init,check,next,action)
  | check(init) = action(init) : forloop(next(init),check,next,action)
  | otherwise = []

-- | Creates the list @[a,f(a),f(f(a)),f(f(f(a))),...]@
iterated :: ((a -> a),a) -> [a]
iterated(f,a) = a : iterated(f,f(a))

-- | Add an element to the front of a list
prepend :: a -> [a] -> [a]
prepend(x)(xs) = x : xs

-- | Add an element at the end of a list
append :: a -> [a] -> [a]
append(x)(xs) = xs ++ [x]

-- | Converts a function @f@ into a list @[f(1),f(2),f(3),..]@
list :: (Number -> a) -> [a]
list(f) = [f(i) | i <- [1..]]

-- | Converts a function @f@ into a finite list @[f(1),f(2),f(3),..f(n)]@
listn :: (Number -> a, Number) -> [a]
listn(f,n) = [f(i) | i <- [1..n]]

-- | Converts a list of elements into a list of pairs of elements
pairs :: [a] -> [(a,a)]
pairs (x:y:rs) = (x,y) : pairs(rs)
pairs _ = []

-- | Converts a list of pairs of elements into a list of elements
unpairs :: [(a,a)] -> [a]
unpairs [] = []
unpairs ((x,y):xys) = x : y : unpairs xys
    
{-
-- | Converts a list of elements into a list of lists of the given length
groups :: ([a],Number) -> [[a]]
groups([],_) = []
groups(rs,n) = first(rs,n) : groups(rest(rs,n),n)
-}

-- | Converts a list of elements into a list of lists of the given length
inGroupsOf :: Number -> [a] -> [[a]]
inGroupsOf(n)(rs) = groups(rs,n)

-- | Converts a list of pairs into a pair of lists
unzipped :: [(a,b)] -> ([a],[b])
unzipped [] = ([],[])
unzipped ((x,y):xys) = (x:xs,y:ys)
    where
    (xs,ys) = unzipped xys
    
-- | Converts a pair of lists into a list of pairs
zipped :: ([a],[b]) -> [(a,b)]
zipped(a:as,b:bs) = (a,b) : zipped(as,bs)
zipped _ = []

-- | Either the index of the first occurrence of @element@ within @list@,
-- or 0 if @element@ does not occur within @list@
indexOf :: (a,[a]) -> Number
indexOf(element,list) = indexOf_(1,list)
  where
  indexOf_(_,[]) = 0
  indexOf_(i,x:xs)
    | element == x = i
    | otherwise = indexOf_(i+1,xs)

-- | Either the index of the first element in list that satisfies @pred@,
-- or 0 if no element satisfies @pred@
find :: (a -> Truth,[a]) -> Number
find(pred,list) = indexOf_(1,list)
  where
  indexOf_(_,[]) = 0
  indexOf_(i,x:xs)
    | pred(x) = i
    | otherwise = indexOf_(i+1,xs)
