{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-------------------------------------------------------------------------------
-- | A set of functions and variables that provide additional support for your
-- programs.
--
-- To use a function defined in this module, you must begin your code with this
-- line:
--
-- > import Extras.Util(function)
--
-- where instead of @function@ you write the actual name of the function you
-- want to use.
--
-- You can specifiy more than one function. For example, if you want to use 3
-- functions, then instead of writing 3 separate @import@ lines, you can write
-- this:
--
-- > import Extras.Util(function1,function2,function3)
--

module Internal.Util
where

import Internal.Prelude
import Internal.Num
import Internal.Picture
import Internal.Text

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified "base" Prelude as P
import           "base" Prelude ((.),($),fst,snd,map)


--------------------------------------------------------------------------------
-- Pictures
--------------------------------------------------------------------------------

-- | A Picture of a dot standing at the origin.
centerDot :: Picture
centerDot = solidCircle(0.1)

-- | @dot(x,y)@ is a 'Picture' of a small dot centered at the 'Point' @(x,y)@.
dot :: Point -> Picture
dot(p) = translated(centerDot,p)

-- | A copy of the given @picture@ that is shown the given number of units
-- to the right of the original one.
right :: (Picture,Number) -> Picture
right(p,n) = translated(p,(n,0))

-- | A copy of the given @picture@ that is shown the given number of units
-- up from the original one.
up :: (Picture,Number) -> Picture
up(p,n) = translated(p,(0,n))

-- | A 'Point' that is the given number of units to the right of the
-- given 'Point'.
rightPoint :: (Point,Number) -> Point
rightPoint((x,y),n) = (x+n, y)

-- | A 'Point' that is the given number of units up from the
-- given 'Point'.
upPoint :: (Point,Number) -> Point
upPoint((x,y),n) = (x, y+n)

--
-- Polygons
--

-- | A list of the edges of the polygonal line determined
-- by the given points. An edge is represented by a pair
-- of points.
polylineEdges :: [Point] -> [(Point,Point)]
polylineEdges (p0:p1:ps) = (p0,p1) : polylineEdges (p1:ps)
polylineEdges _ = []

-- | A list of the edges of the closed polygon determined
-- by the given points. An edge is represented by a pair
-- of points.
polygonEdges :: [Point] -> [(Point,Point)]
polygonEdges [] = []
polygonEdges(p0:ps) = go p0 ps
    where
    go p [] = [(p,p0)]
    go p (p1:ps) = (p,p1) : go p1 ps

edgeLength :: (Point,Point) -> Number
edgeLength((x0,y0),(x1,y1)) = sqrt((x0-x1)^2 + (y0-y1)^2)

-- | The perimeter of the polygon determined by the given points
polygonPerimeter :: [Point] -> Number
polygonPerimeter(pts) = go (polygonEdges pts)
    where
    go [] = 0
    go (e:es) = edgeLength e + go es

-- | The area of the polygon determined by the given points.
-- If the polygon intersects itself, the area calculated by
-- this function may not be what you expected, because there
-- are several incompatible ways to calculate areas of
-- self-intersecting polygons.
polygonArea :: [Point] -> Number
polygonArea(pts) = abs(sum(map area (polygonEdges pts))) / 2
    where
    area ((x0,y0),(x1,y1)) = x0*y1 - x1*y0

-- | The horizontal length (width) and the vertical length (height) of
-- the smallest rectangular box that would fit the given vertices of
-- a polygon.
polygonBounds :: [Point] -> (Number,Number)
polygonBounds(pts) = go (unzipped pts)
    where
    go(xs,ys) = (bound xs, bound ys)
    bound vs = maximum vs - minimum vs

-- | The coordinates of the
-- geometric center (cf. https://en.wikipedia.org/wiki/Centroid)
-- of the given points. This function will fail if no points are given.
polygonCenter :: [Point] -> Point
polygonCenter = \case
    [] -> error "An empty polygon has no center"
    pts -> go (unzipped pts)
    where
    go(xs,ys) = (avg xs,avg ys)
    avg vs = sum vs / length vs

-- | The total length of the polygonal line determined by the given points.
polylineLength :: [Point] -> Number
polylineLength(pts) = sum(map edgeLength (polylineEdges pts))

-- | A list of triples where each vertex of a polygon is listed along with the
-- preceding and the succeeding vertex. The order of vertices in each triple is
-- @(preceeding,current,succeeding)@.
polygonCorners :: [Point] -> [(Point,Point,Point)]
polygonCorners([]) = []
polygonCorners(points) = plast : pother
  where
  (plast,pother) = recycle(points)(corners)
  corners = go(repeating(points))
  go(a:b:c:ps) = (a,b,c) : go(b:c:ps)
  recycle([p])(c:cs) = (c,[])
  recycle(p:ps)(c:cs) = (c',c:cs')
    where
    (c',cs') = recycle(ps)(cs)

-- | This function takes a list of corners, as calculated by 'polygonCorners',
-- and returns the angles for an arc that spans the corresponding
-- vertex. If the list of corners is given so that the interior of the polygon
-- is at the left when traversing it, then the arcs will correspond to the
-- interior angles of the polygon.
cornerAngles :: (Point,Point,Point) -> (Number,Number)
cornerAngles(a,b,c) = corner(ediff(a,b),ediff(b,c))
  where
  ediff(p,q) = vectorDifference(q,p)
  corner(ein,eout) = diff(vectorDirection(ein),vectorDirection(eout))
  diff(a,b) = step2(step1(a,b))
    where
    step1(a,b) = if b < a then (a,b+360) else (a,b)
    step2(a,b) = if b - a < 180 then (b,a+180) else (b-360,a+180)

--------------------------------------------------------------------------------
-- Animation
--------------------------------------------------------------------------------

-- | You can simulate motion by specifying slightly different pictures at different
-- instants in time (measured in seconds). Thus, an animation is just a function
-- of time that specifies which picture to show at each time.
type Animation = Number -> Picture

-- | The expression @saw(t,p)@ is @0@
-- when @t=0@, increases up to 1 when @t=p/2@, and then decreases back
-- to 0 when @t=p@.
-- This increasing and decreasing when @t@ goes from @0@ to @p@ is called
-- an oscillation of period @p@. The oscillations will keep repeating,
-- so that the function is @0@ when @t@ is @0,p,2p,3p,4p,5p,...@
-- and it is 1 when @t@ is @p/2@, @3p/2@, @5p/2@, @7p/2@, @...@
saw :: (Number,Number) -> Number
saw(t,p) = 1 - abs(2*abs(remainder(t,p))/p - 1)

-- | An animation that just shows the same static picture at all times
staticMotion :: Picture -> Animation
staticMotion(pic)(t) = pic

-- | @repeatedMotion(period,motion)@ repeats the given @motion@ every
-- @period@ seconds. The @motion@ should begin at time 0 and end at time 1.
repeatedMotion :: (Number,Animation) -> Animation
repeatedMotion(period,motion)(t) = motion(saw(t,period))

-- | @delayedMotion(delay,motion)@ delays the start of the given @motion@
-- until @dealy@ seconds have passed.
delayedMotion :: (Number,Animation) -> Animation
delayedMotion(delay,motion)(t) = if t < delay then blank else motion(t-delay)

-- | @fasterMotion(factor,motion)@ will play the given @motion@ @factor@ times
-- faster than normal. If @factor@ is a number between 0 and 1, then @motion@
-- will actually play slower than normal.
fasterMotion :: (Number,Animation) -> Animation
fasterMotion(factor,motion)(t) = motion(factor*t)

-- | Apply the given transformation to an animation. Since most transformations
-- have additional parameters, you will need to use the functions 'fixed1' or
-- 'fixed2' to add those parameters.
--
-- Example:
--
-- shiftedRight = transformedMotion(fixed2(right,5)) -- move an animation to the right
--
-- animation1 = circle -- A circle centered at the origin that grows with time
--
-- animation2 = shiftedRight(animation1) -- A circle centered at (5,0) that grows
--
transformedMotion :: (Picture -> Picture) -> (Animation -> Animation)
transformedMotion(trans)(motion)(t) = trans(motion(t))

-- | This function is similar to 'combined', but it combines animations
combinedMotions :: [Animation] -> Animation
combinedMotions(moves)(t) = combined(distributed(execute,moves))
  where
  execute(move) = move(t)

-- | @showBetween(start,finish,motion)@ shows @motion@ only when time is
-- between @start@ and @finish@. The @motion@ should be defined
-- so that it begins at time 0 and ends at time 1. It will then be
-- automatically adjusted, so that it begins at time @start@ and ends at
-- time @finish@.
showBetween :: (Number,Number,Animation) -> Animation
showBetween(ini,fin,motion)(t)
    | ini <= t && t < ini = motion( (t-ini) / (fin-ini) )
    | otherwise = blank

-- | This function is similar to 'showBetween', but it will show
-- the initial picture @motion(0)@ before the start time, and it
-- will show the final picture @motion(1)@ after the finish time.
travelBetween :: (Number,Number,Animation) -> Animation
travelBetween(ini, fin, motion)(t) 
    | t < ini = motion(0)
    | ini <= t && t < fin = motion( (t-ini) / (fin-ini) )
    | fin <= t = motion(1)

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

-- | @cloned(object)@ is an infinite list of clones of the given @object@
cloned :: object -> [object]
cloned(o) = repeating([o])

-- | @distributed(transformation,objects)@ is a list of
-- objects, where each object is created by applying the given
-- 'transformation' to each object in the given list of 'objects'.
distributed :: ((a -> b), [a]) -> [b]
distributed(f,xs) = map f xs

-- | @iterated(transform,[parameter0,parameter1,...])@ is a list
-- where each element
-- is the result of applying the given @transform@ with a
-- @parameter@ to the previous element. The given parameter list is
-- consumed sequentially, so that each new application
-- uses a new @parameter@.
-- When an initial @object0@ comes from a pipeline, the result is
-- @[object0, object1, object2, ... ]@
-- where
-- @object1@ is @transform(object0, parameter0)@,
-- @object2@ is @transform(object1, parameter1)@
-- and so on.
iterated :: ((object,param) -> object,[param]) -> object -> [object]
iterated(f,[])(o) = [o]
iterated(f,p:ps)(o) = o : iterated(f,ps)(next)
    where
    next = f(o,p)

-- | Select the second argument when the first argument is True,
-- and select the third argument when the first argument is False
on :: (Truth,value,value) -> value
on(True,accept,_) = accept
on(False,_,reject) = reject

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

type Params = M.Map T.Text Number

newParams :: Params
newParams = M.empty

set :: (Text,Number) -> Params -> Params
set(k,v) = M.insert (fromCWText k) v

get :: (Text,Number) -> Params -> Number
get(k,v) = M.findWithDefault v (fromCWText k)

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------

-- | A predicate is a function that separates all possible values of a given
-- type @t@ into two groups: those for which the predicate is @True@ and those
-- for which the predicate is @False@ when it is applied to them.
type Predicate t = t -> Truth

-- | A list of values selected from the given list. A value
-- is selected if it satisfies the given predicate. Otherwise, it is discarded.
selected :: ([value], Predicate value) -> [value]
selected(ls,f) = P.filter f ls

-- | A list of values selected form the given key-value list. A value
-- is selected if the corresponding key satisfies the given predicate.
-- This function is useful in lookup tables.
selectedValues :: ([(key,value)], Predicate key) -> [value]
selectedValues(kvList, pred) = P.map snd . P.filter (pred.fst) $ kvList

-- | A list of keys selected form the given key-value list. A key
-- is selected if the corresponding value satisfies the given predicate.
-- This function is useful to do /reverse lookups/ in tables.
selectedKeys :: ([(key,value)], Predicate value) -> [key]
selectedKeys(kvList, pred) = P.map fst . P.filter (pred.snd) $ kvList

-- | A predicate that holds whenever all the given predicates
-- hold.
-- The trailing underscore in the function name is included
-- to distinguish it from the predicate named 'all'.
all_ :: [Predicate value] -> Predicate value
all_(preds)(v) = all(P.map test preds)
  where
  test(pred) = pred(v)

-- | A predicate that holds whenever at least one of the given
-- predicates holds.
-- The trailing underscore in the function name is included
-- to distinguish it from the predicate named 'any'.
any_ :: [Predicate value] -> Predicate value
any_(preds)(v) = any(P.map test preds)
  where
  test(pred) = pred(v)

-- | @choices(options)@ converts a given input into
-- a list of outputs according to the rules given.
-- Each rule has a predicate and an output value.
-- If the input satisfies a predicate, the corresponding
-- output value is included in the result.
--
-- Example:
--
-- > test = choices([ (above(100),"You have fever")
-- >                , (beween(97,100), "Your temperature is normal")
-- >                , (below(97), "You have hypothermia")
-- >                , (between(96,110), "You are alive")
-- >                ])
-- > 
-- > message = test(102) -- ["You have fever", "You are alive"]
-- >
--
choices :: [(Predicate inv, outv)] -> inv -> [outv]
choices(options)(v) = selectedValues(options,pred)
  where
  pred(test) = test(v)

-- | @choice(rules,default)@ is similar to 'choices', but
-- it produces just the first result that matches the rules.
-- If no rule matches, then the result is the given default.
choice :: ([(Predicate inv, outv)], outv) -> inv -> outv
choice(opts,def)(v) = case choices(opts)(v) of
                       [] -> def
                       other -> other#1

-- Specific Predicates

-- | @precedes(text1,text2)@ is @True@ whenever @text1@ precedes @text2@
-- alphabetically. It is @False@ otherwise.
-- When the two texts are the same,
-- the result of @precedes@ is @False@.
precedes :: Predicate(Text,Text)
precedes(a,b) = fromCWText a P.< fromCWText b

-- | A predicate that can be applied to an argument to check whether
-- it is contained in the given @list@.
--
-- Example:
--
-- > selected([1..10],is_in([0,2..100]) -- is [2,4,6,8,10]
--
is_in :: [value] -> Predicate value
is_in(list)(x) = contains(list,x)

-- | A predicate that is @True@ when the argument is a non-empty list.
nonEmpty :: Predicate [value]
nonEmpty = not.empty

-- | A predicate that is @True@ when the argument is between
-- the first parameter (included) and the second parameter (excluded).
-- In other words, @between(a,b)(value)@ holds whenever
-- @a <= value@ and @value < b@
between :: (Number,Number) -> Predicate Number
between(a,b)(v) = a <= v && v < b

-- | A predicate that is @True@ when the argument is above
-- the given parameter (included).
-- In other words, @above(b)(value)@ holds whenever
-- @b <= value@
above :: Number -> Predicate Number
above(b)(v) = b <= v

-- | A predicate that is @True@ when the argument is below
-- the given parameter (excluded).
-- In other words, @below(a)(value)@ holds whenever
-- @value < a@
below :: Number -> Predicate Number
below(a)(v) = v < a

-- | A predicate that is @True@ when the argument is equal to
-- the given parameter.
-- In other words, @exactly(a)(value)@ holds whenever
-- @value == a@
exactly :: value -> Predicate value
exactly(a)(v) = a == v

-- | A predicate that is @True@ whenever the given predicate
-- is @False@.
-- In other words, @excluded(pred)(value)@ holds whenever
-- @pred(value)@ does not hold.
excluded :: Predicate value -> Predicate value
excluded(pred)(v) = not(pred(v))



-------------------------------------------------------------------------------
-- Grouping and Sorting
-------------------------------------------------------------------------------

-- | @alphabeticalSortedOn(key, list)@ is a list that has the same values as
-- the given @list@, but the values are sorted in the
-- following way: if @key(value1)@ precedes @key(value2)@ alphabetically
-- then @value1@ will apear before @value2@
alphabeticalSortedOn :: (value -> Text, [value]) -> [value]
alphabeticalSortedOn(k,l) = L.sortOn (fromCWText . k) l

-- | @numericalSortedOn(key, list)@ is a list that has the same values as the
-- given @list@, but the values are sorted in the following
-- way: if @key(value1) < key(value2)@ then @value1@ will apear before
-- @value2@
numericalSortedOn :: (value -> Number, [value]) -> [value]
numericalSortedOn(k,l) = L.sortOn k l

-- |@logicalGroupBy(predicate, list)@ breaks up the given @list@ into
-- two sublists: one with the elements that satisfy the given @predicate@
-- and another with the elements that do not satisfy the given @predicate@.
-- Neither the order of the elements in each
-- sublist nor the order of each sublist in the output list is specified,
-- so you cannot make any assumption about the order in which elements
-- will be present in the result.
--
-- Example:
--
-- > logicalGroupBy(even, [1,2,3,4]) -- is [ (True,[2,4]), (False,[1,3]) ]
--
logicalGroupBy :: (Predicate value, [value]) -> [ (Truth,[value]) ]
logicalGroupBy(pred,list) = [ (True,yes), (False,no) ]
    where
    (yes,no) = L.partition pred list

-- |@alphabeticalGroupBy(key, list)@ breaks up the given @list@ into sublists
-- that have the same @key@.
-- Neither the order of the elements in each
-- sublist nor the order of each sublist in the output list is specified,
-- so you cannot make any assumption about the order in which elements
-- will be present in the output list.
--
alphabeticalGroupBy :: (value -> Text, [value]) -> [(Text,[value])]
alphabeticalGroupBy(key, list) = apply(list)
    where
    apply = groupBy . aSortOn P.fst . P.map build
    aSortOn(p)(x) = alphabeticalSortedOn(p,x)
    build x = (key x,x)

-- |@numericalGroupBy(key, list)@ breaks up the given @list@ into sublists
-- that have the same @key@.
-- Neither the order of the elements in each
-- sublist nor the order of each sublist in the output list is specified,
-- so you cannot make any assumption about the order in which elements
-- will be present in the output list.
--
numericalGroupBy :: (value -> Number, [value]) -> [(Number,[value])]
numericalGroupBy(key, list) = apply(list)
    where
    apply = groupBy . nSortOn P.fst . P.map build
    nSortOn(p)(x) = numericalSortedOn(p,x)
    build x = (key x,x)

-- Not exported
groupBy :: [(a,b)] -> [(a,[b])]
groupBy [] = []
groupBy ((x,d):xds) = (x,d:ds) : groupBy xds_ne
    where
    (xds_eq,xds_ne) = L.span ((x ==) . P.fst) xds
    ds = P.map P.snd xds_eq

-------------------------------------------------------------------------------
--- Control flow
-------------------------------------------------------------------------------

-- | Run a sequence of transformations in order on the given initial state.
-- For example, the expression @run([f1,f2,f3])(x)@
-- is the same as @f3(f2(f1(x)))@
run :: [value -> value] -> value -> value
run([])(x) = x
run(f:fs)(x) = run(fs)(f(x))

-- | Repeat a transformation the given number of times.
-- For example, the expression @repeatFor(3,f)(x)@ is the same as @f(f(f(x)))@.
-- If you use a negative number or a number with decimals, the sign and
-- the decimals will be ignored. For example, @repeatFor(-7.3,f)@ will repeat
-- @7@ times.
repeatFor :: (Number,value -> value) -> value -> value
repeatFor(n,f) = go (truncated(abs(n)))
    where
    go 0 x = x
    go n x = go (n-1) (f x)

-- | Keep repeating a transformation while the given predicate is True.
-- The result is the first value that does not satisfy the predicate.
repeatWhile :: (Predicate value, value -> value) -> value -> value
repeatWhile(cond,next)(input) = go input
    where
    go x = if cond x then go(next x) else x

-- | Constructs a list by applying a function
-- to all the elements of a given list
foreach :: ([input],input -> output) -> [output]
foreach(l,f) = P.map f l

-- | Creates the list @[a,f(a),f(f(a)),f(f(f(a))),...]@,
-- where @a@ is the given value and @f@ is the given function.
-- iterated :: ((value -> value), value) -> [value]
-- iterated(next,input) = input : iterated(next,next input)

-- | @forloop(input,cond,next,output)@ produces a list of outputs,
-- where each output is generated by repeatedly applying @output@
-- to the current @state@, whose value changes after each iteration
-- of the loop. The @state@ is initially set to the value given by
-- @input@, and new states are generated from it by applying
-- @next@ to the current @state@.
-- The whole process continues for as long as the current @state@
-- satisfies the predicate given by @cond@.
--
-- For example, the following code will produce [0,0,6,6,12,12,20,20]:
--
-- > forloop(input,cond,next,output)
-- >     where
-- >     input         = (1,0)
-- >     cond(n,sum)   = n <= 10
-- >     next(n,sum)   = (n+1,if even(n) then sum+n else sum)
-- >     output(n,sum) = sum
--
-- = Using forloop to implement other iteration functions
--
-- Basically, any iteration function can be used to implement the rest.
-- A few examples of implementations based on @forloop@ follow.
--
-- 'iterated':
--
-- > iterated(next,input) = forloop(input,\x -> True,next,pass)
--
-- 'foreach':
--
-- > foreach(list,f) = forloop(list,nonEmpty,\l -> rest(l,1),\l -> f(l#1))
--
-- 'repeatWhile':
--
-- > repeatWhile(check,f)(v) = last(v:forloop(input,cond,next,output),1)#1
-- >     where
-- >     input        = (x,f(x))
-- >     cond  (x, _) = check(x)
-- >     next  (_,fx) = (fx,f(fx))
-- >     output(_,fx) = fx
--
forloop :: (state, Predicate state, state -> state, state -> output)
        -> [output]
forloop(input,cond,next,output)
  | cond(input) = output(input) : forloop(next(input),cond,next,output)
  | otherwise = []


-- | The function @whileloop@ works similarly to 'forloop', but instead
-- of collecting outputs of intermediate states, a single output is collected
-- at the end of the loop. The expression @whileloop(input,cond,next,output)@
-- is a shortcode for @output(repeatWhile(cond,next)(input))@.
--
-- Example 1. The function 'indexOf' can be implemented as a /while loop/:
--
-- > indexOf(x,list) = whileloop(input,cond,next,output)
-- >   where
-- >   input                    = (list,1,0)
-- >   cond(list,index,found)   = nonEmpty(list) && found == 0
-- >   next(list,index,found)   = ( rest(list,1)
-- >                              , index+1
-- >                              , if x == list#1 then index else found
-- >                              )
-- >   output(list,index,found) = found
--
-- Example 2. The average of a list of numbers can be calculated by the
-- following /while loop/:
--
-- > average(numbers) = whileloop(input,cond,next,output)
-- >   where
-- >   input                        = (numbers, 0, 0)
-- >   cond(numbers,_,_)            = nonEmpty(numbers)
-- >   next(numbers,total,quantity) = ( rest(numbers,1)
-- >                                  , total + numbers#1
-- >                                  , quantity + 1
-- >                                  )
-- >   output(_,_,0)                = 0 -- adjust this as needed
-- >   output(_,total,quantity)     = total / quantity
--
-- Example 3. The function 'forloop' can be implemented
-- in terms of @whileloop@:
--
-- > forloop(input,cond,next,output) = whileloop(input',cond',next',output')
-- >   where
-- >   input'           = (input,[])
-- >   cond'(s,accum)   = cond(s)
-- >   next'(s,accum)   = (next(s),prepend(output(s),accum))
-- >   output'(s,accum) = reversed(accum)
--
-- We could have used 'append' instead of 'prepend', so that the accumulator
-- does not need to be reversed at the end. However, due to internal
-- implementation details of CodeWorld, the latter is much more efficient.
--
whileloop :: (state,Predicate state,state -> state, state -> output)
          -> output
whileloop(input,cond,next,output) = output(repeatWhile(cond,next)(input))

-------------------------------------------------------------------------------
-- List manipulation
-------------------------------------------------------------------------------

-- | Add a value to the front of a list
prepend :: (value, [value]) -> [value]
prepend(x, xs) = x : xs

-- | Add a value at the end of a list
append :: (value, [value]) -> [value]
append(x,xs) = xs ++ [x]

-- | Converts a given function @f@ into a list @[f(1),f(2),f(3),...]@
list :: (Number -> value) -> [value]
list(f) = [f(i) | i <- [1..]]

-- | Converts a given function @f@ into a finite
-- list @[f(1),f(2),f(3),...,f(n)]@,
-- where @n@ is the given number.
-- If you use a Number with decimals,
-- the decimals will be ignored.
listn :: (Number -> value, Number) -> [value]
listn(f,n) = [f(i) | i <- [1..truncated(n)]]

-- | A list containing all the elements of the given list except the
-- last few. Example: @butLast([1,2,3,4],1)@ is @[1,2,3]@.
butLast :: ([value],Number) -> [value]
butLast(ls,num)
  | num > len = []
  | otherwise = first(ls,len-num)
  where
  len = length(ls)

-- | A list of pairs that is created by
-- putting together each consecutive pair of values in the given
-- list. Single elements at the end of the list are discarded.
--
-- Example:
--
-- > pairs([1..9]) -- is [(1,2),(3,4),(5,6),(7,8)]
--
pairs :: [value] -> [(value,value)]
pairs (x:y:rs) = (x,y) : pairs(rs)
pairs _ = []

-- | A list of values obtained by flattening the given
-- list of pairs, so that the overall order of the values
-- is preserved.
unpairs :: [(value,value)] -> [value]
unpairs [] = []
unpairs ((x,y):xys) = x : y : unpairs xys
    
-- | A pair of lists obtained by separating each pair
-- from the given list of pairs.
unzipped :: [(a,b)] -> ([a],[b])
unzipped [] = ([],[])
unzipped ((x,y):xys) = (x:xs,y:ys)
    where
    (xs,ys) = unzipped xys
    
-- | A list of pairs that results from blending the
-- given pair of lists, by taking one value from
-- each list at a time. The resulting list is as
-- short as the shortest of the two given lists.
-- When one of the lists is longer than the other,
-- the extra values will be discarded.
zipped :: ([a],[b]) -> [(a,b)]
zipped(a:as,b:bs) = (a,b) : zipped(as,bs)
zipped _ = []

-- | Either the index of the first occurrence of the given value within
-- the given list, or 0 if the value does not occur in the list. List
-- indices start at 1.
indexOf :: (value,[value]) -> Number
indexOf(element,list) = indexOf_(1,list)
  where
  indexOf_(_,[]) = 0
  indexOf_(i,x:xs)
    | element == x = i
    | otherwise = indexOf_(i+1,xs)

-------------------------------------------------------------------------------
-- Text formatting
-------------------------------------------------------------------------------

-- | Justifies text to the left, and adds padding on the right, so that the
-- text occupies exactly the given width. Justification works best
-- with lettering based on monospaced fonts.
lJustified :: (Text,Number) -> Text
lJustified(txt,width)
  | len < width = lJustified(txt <> " ",width)
  | len > width = cut
  | otherwise = joined(last(characters(txt),width))
  where
  len = numberOfCharacters(txt)
  cut = joined(first(characters(txt),width))
  
-- | Justifies text to the right, and adds padding on the left, so that the
-- text occupies exactly the given width. Justification works best
-- with lettering based on monospaced fonts.
rJustified :: (Text,Number) -> Text
rJustified(txt,width)
  | len < width = rJustified(" " <> txt,width)
  | len > width = cut
  | otherwise = joined(last(characters(txt),width))
  where
  len = numberOfCharacters(txt)
  cut = joined(last(characters(txt),width))
  
-- | A text representation of the given number, so that it has
-- exactly the given number of decimals. This means that the output
-- does not represent the given number exactly, but a number that is only
-- approximately equal to the given number. When the number of decimals
-- requested is 1 or more, a decimal point is also included in the text
-- representation, followed or preceded by 0 if necessary.
-- 
-- > printedDecimals(7.89,1) -- is "7.9"
-- 
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
                                                                       
-- | A text representation of the given list of numbers.
printedNumbers :: [Number] -> Text
printedNumbers(list) = "[" <> printedRawNumbers(list) <> "]"
  where
  printedRawNumbers[] = ""
  printedRawNumbers[n] = printed(n)
  printedRawNumbers(n:ns) = printed(n) <> ", " <> printedRawNumbers(ns)

-- | A text representation of the given point.
printedPoint :: Point -> Text
printedPoint(x,y) = "(" <> printed(x) <> "," <> printed(y) <> ")"

-- | A picture that represents the given list of texts, so that each
-- text in the list is shown in a separate line. Lines start at the
-- top left corner of the output window and grow downward.
-- Each line of text can fit 66 characters, and 40 lines can fit
-- in a single page. The lettering is shown in monospaced font.
letteringBlock :: [Text] -> Picture
letteringBlock(lines) = combined([showline(i) | i <- [1..n]])
    where
    n = length(lines)
    -- Output should be 40 rows and 66 columns
    maxChars = min(66,maximum(P.map numberOfCharacters lines))
    showline(i) = translated(scaled(fmt(lines#i),0.5,0.5),(hShift,vShift i))
    hShift = -10+5/33*maxChars
    vShift i = 10.25-0.5*i
    fmt(txt) = styledLettering(lJustified(txt,maxChars),Monospace,Italic)

-- | The horizontal length (width) and the vertical length (height) of
-- the output produced by @letteringBlock@ on the same input, so that you can
-- place it at precise locations and add snuggly fit decorations to the text.
letteringBlockBounds :: [Text] -> (Number,Number)
letteringBlockBounds(texts) =
    (maximum(P.map numberOfCharacters texts)*10/33, length(texts)/2)

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

-- | A list of cumulative sums calculated from the given list.
--
-- > cumulativeSums([1,2,3,4]) -- is [1,1+2,1+2+3,1+2+3+4]
--
-- Note that @cumulativeSums@ could be implemented using @forloop@ as follows:
--
-- > cumulativeSums(list) = forloop(init,cond,next,output)
-- >     where
-- >     init   =                       (list,0)
-- >     cond   = \(list,currentSum) -> nonEmpty(list)
-- >     next   = \(list,currentSum) -> (rest(list,1),currentSum + list#1)
-- >     output = \(list,currentSum) -> currentSum + list#1
--
cumulativeSums :: [Number] -> [Number]
cumulativeSums = P.tail . P.scanl (+) 0

-- | A function that passes the input unchanged as output. It is useful to
-- indicate a transformation that /changes nothing/ for those operations
-- where a transformation is expected.
pass :: value -> value
pass = P.id

-- | Creates a number out of a text in such a way
-- that 
--
-- (1) it is difficult to predict the number
-- corresponding to a given text, and
-- (2) it is
-- relatively unlikely that two similar texts
-- have similar numbers.
--
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
    other = " .,'!@#$%^&*()-=_+|/<>\\0123456789"

