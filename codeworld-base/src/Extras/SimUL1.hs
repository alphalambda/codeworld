{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Extras.SimUL1
       (Params(..), motionParams, motionOf)
where

import Prelude

-------------------------------------------------------------------------------
--- Code for starting the program
-------------------------------------------------------------------------------

data Params = Params
    { use_clicks :: Truth
    , landmarks :: [(Text,Number)]
    , time_max :: Number
    , position_min :: Number
    , position_max :: Number
    , velocity_min :: Number
    , velocity_max :: Number
    , strobe_period :: Number
    , strobe_mindist :: Number
    , strobe_target :: Text
    , position_label :: Text
    , velocity_label :: Text
    , update_iterations :: Number
    , update_speedup :: Number
    , segments :: Number
    }

motionParams = Params
    { use_clicks = False
    , landmarks = []
    , time_max = 100
    , position_min = 0
    , position_max = 100
    , velocity_min = 0
    , velocity_max = 100
    , strobe_period = 10
    , strobe_mindist = 5
    , strobe_target = ""
    , position_label = "position"
    , velocity_label = "velocity"
    , update_iterations = 2
    , update_speedup = 1
    , segments = 10
    }

motionOf(model_data,velocity_function,params) =
    if use_clicks(params) then interactionOf(init,update',handle,render)
    else simulationOf(init,update',render)
    where
    init = initial_model model_data velocity_function params
    update' = speeded(update_speedup params,repeated_update(update_iterations params))

-------------------------------------------------------------------------------
--- Code for describing the kinematic model
-------------------------------------------------------------------------------

type Position = Number
type Velocity = Number
type Time = Number
type Click = Maybe (Number,Number)

type ModelData = [(Position,Color,Color -> Picture,Text)]
type VelocityFunction = (Text,Number) -> Number

data Model = Model
  { time :: Time
  , states :: [State]
  , vf :: VelocityFunction
  , click :: Click
  , rs :: [Number]
  , params :: Params
  , sim :: SimLine
  , pchart :: Chart
  , vchart :: Chart
  }

initial_model :: ModelData -> VelocityFunction -> Params -> [Number] -> Model      
initial_model model_data velocity_function params rs = Model
  { time = 0
  , states = [ make_state s | s <- model_data ]
  , vf = velocity_function
  , click = Nothing
  , rs = rs
  , params = params
  , sim = make_sim(1,position_min params,position_max params,landmarks params,segments params)
  , pchart = make_chart(-5,-5,position_label params,time_max params,position_min params,position_max params)
  , vchart = make_chart(5,-5,velocity_label params,time_max params,velocity_min params,velocity_max params)
  }
  where
  make_state (p,c,s,n) =
      State {
        position = p,
        velocity = v,
        color = c,
        shape = s(c),
        name = n,
        strobe = [],
        pgraph = [(0,p)],
        vgraph = [(0,v)]}
      where
      v = velocity_function(n,0)

speeded(times,updater) = updater'
    where
    updater'(model,dtime) = updater(model,times*dtime)

repeated_update :: Number -> (Model,Time) -> Model
repeated_update n = update
    where
    update(model,dtime)
        | time(model) >= time_max (params model) = model
        | otherwise = update_output(partial_update(n,model))
        where
        dt = dtime/n
        partial_update(i,m) =
           if i <= 0 then m
           else partial_update(i-1,update_model(m,dt))

update_model :: (Model,Time) -> Model  
update_model(model,dtime) =
    model { time = time'
          , states = [updated_state s | s <- states(model)]
          }
    where
    velocity_function = vf(model)
    time' = time(model) + dtime
    updated_state(state) =
      let
        position' = position(state) + velocity'/60.0 * dtime
        velocity' = velocity_function(name(state),time(model))
      in
        state { position = position'
              , velocity = velocity'
              }

update_output :: Model -> Model
update_output model | t >= time_max(params model) = model
                    | otherwise = model { states = [ updated_state_output sp sm st t s 
                                                   | s <- states(model)] }
    where
    t = time(model)
    sp = strobe_period (params model)
    sm = strobe_mindist (params model)
    st = strobe_target (params model)

data State = State {
    position :: Position,
    velocity :: Velocity,
    shape :: Picture,
    color :: Color,
    name :: Text,
    strobe :: [(Time,Position,Velocity)],
    pgraph :: [(Time,Position)],
    vgraph :: [(Time,Velocity)]
    }


updated_state_output sp sm st t state =
  let
    p = position(state)
    v = velocity(state)
    ss = strobe(state)
    nm = name(state)
{-
    at_strobe_time = isInteger(rounded(t*10)/(10*sp))
    ok_strobe = empty(ss) || abs(secondOfPair(ss#0)-p) > sm
    strobe'
      | at_strobe_time && ok_strobe = (t,p) : ss
      | otherwise = ss
-}
    pgraph' = (t,p) : pgraph(state)
    vgraph' = (t,v) : vgraph(state)
  in
    state {
      strobe = if st == nm then update_strobe sp sm t p v ss else [],
      pgraph = pgraph',
      vgraph = vgraph'
      }

update_strobe sp sm t p v [] = [(t,p,v)]
update_strobe sp sm t p v ss@((last_t,last_p,_):_)
    | t - last_t >= sp, abs(p - last_p) >= sm = (t,p,v):ss
    | otherwise = ss

-------------------------------------------------------------------------------
--- Code for creating and displaying the simulation line
-------------------------------------------------------------------------------

data SimLine = SimLine { render_line :: Picture
                       , render_shape :: (Position,Velocity,Picture) -> Picture
                       , render_strobe :: ([(Time,Position,Velocity)],Color) -> Picture
                       , within_sim :: Point -> Truth
                       , to_sim :: Point -> Point
                       }
                       
make_sim(simrow,x_min,x_max,landmarks,parts) =
  let
    x_hrange = 0.6 * (x_max - x_min)
    x_center = 0.5 * (x_max + x_min)

    dx = (x_max-x_min)/parts
    
    render_mark(m) =
      translated(path[(0,0.2),(0,-0.2)],screen_coords(x_min+m*dx),0)
      
    render_line =
      let
        origin = screen_coords(x_min)
        destin = screen_coords(x_max)
        simline = path [(-10,0),(10,0)]
                & pictures[render_mark(m) | m <- [0..parts]]
      in
        blank
        & colored(translated(simline,0,simrow),green)
        & pictures[translated(minitext(lname,0.5),screen_coords(lpos),simrow-0.5)
                  | (lname,lpos) <- landmarks
                  ]
        
    render_shape(position,velo,shape) =
        translated(shape',xpos,simrow)
        where
        shape' | velo < 0 = scaled(shape,-1,1)
               | otherwise = shape
        xpos = screen_coords(position)

    render_strobe(strobe,color) =
      let
        render_single(time,position,velo) =
              colored(translated(txt,screen_coords(position),simrow+1+number),color)
              where
              number = if velo >= 0 then 0 else 1
              txt = scaled(text(txt3),0.6,0.6)
              txt1 = approximate(time)
              txt2 = characters(txt1)
              txt3 = joined(first(txt2,length(txt2)-2))
      in
        pictures ([render_single s | s <- strobe])
        
    within_sim (x,y) = abs(y-simrow) < 1
  
    screen_coords(position) = 10 * (position - x_center) / x_hrange
    
    to_sim (x,y) =
      let
        x' = x_center + x_hrange * x / 10
      in
        (x',0)
  in
    SimLine render_line render_shape render_strobe within_sim to_sim


-------------------------------------------------------------------------------
--- Code for creating and displaying charts
-------------------------------------------------------------------------------

data Chart = Chart { render_frame :: Picture
                   , render_graph :: ([Point],Color) -> Picture
                   , within_graph :: Point -> Truth
                   , to_graph :: Point -> Point
                   }
                   
make_chart(ct,cx,ylabel,t_max,x_min,x_max) =
  let
    chart_size = 7.5
    halfsize = 0.5*chart_size
    st = chart_size/t_max
    sx = halfsize / x_hrange
    x_hrange = 0.5 * (x_max - x_min)
    x_center = 0.5 * (x_max + x_min)
    
    render_frame =
      let
        labelpos = 0.55 * chart_size
        chart =
            rectangle(chart_size,chart_size)
            & translated(minilabel(0,0.4),-halfsize+0.2,-halfsize-0.2)
            & translated(minilabel(t_max,0.4),halfsize-0.2,-halfsize-0.2)
            & translated(minilabel(x_min,0.4),-halfsize-0.5,-halfsize+0.2)
            & translated(minilabel(x_max,0.4),-halfsize-0.5,halfsize-0.2)
            & translated(scaled(text("time"),0.8,0.8),0,-labelpos)
            & translated(rotated(scaled(text(ylabel),0.8,0.8),90),-labelpos,0)
      in
        colored(translated(chart,ct,cx),black)

    render_graph(pts,color) = colored(path[screen_coords p | p <- pts],color)

    within_graph (t,x) = abs(t-ct) < halfsize && abs(x-cx) < halfsize
    
    screen_coords (t,x) = (st*t+ct-halfsize, sx*(x-x_center)+cx)

    to_graph (t,x) = ((t-ct+halfsize) / st, x_center + (x-cx) / sx)
  in
    Chart render_frame render_graph within_graph to_graph


-------------------------------------------------------------------------------
--- Code for displaying everything on the screen
-------------------------------------------------------------------------------

render(model) =
    blank
    & render_info(time(model),click(model),nstates)
    & pictures [render_state(i,s) | s <- states model | i <- [0..nstates-1]]
    & render_line sim'
    & render_frame pchart'
    & render_frame vchart'
    where
    nstates = length(states(model))
    sim' = sim model
    pchart' = pchart model
    vchart' = vchart model

    may_render_strobe(state,number)
        | strobe_target(params model) == name(state) =
                render_strobe(sim')(strobe(state),color(state))
        | otherwise = blank

    render_state(number,state) = blank
        & render_shape(sim')(position(state),velocity(state),shape(state))
        & may_render_strobe(state,number)
        & render_graph(pchart') (pgraph(state),color(state))
        & render_graph(vchart') (vgraph(state),color(state))
        & colored(render_data(name(state),
                              position(state),
                              velocity(state),
                              number),
                  color(state))


-------------------------------------------------------------------------------
--- Code for creating and displaying the panels
-------------------------------------------------------------------------------

render_info(time,click,nstates) =
    blank
    -- time box
    & translated(text("time"),-8,9)
    & translated(text(approximate(time)),-8,8)
    & translated(rectangle(2,2),-8,8.5)
    -- click box
    & render_click(click)
    -- data box
    & translated(scaled(text("position"),0.6,0.6),-0.5,9)
    & translated(scaled(text("velocity"),0.6,0.6),2,9)
    & translated(rectangle(10,1+nstates),-1,9-0.5*nstates)

render_click(Nothing)= blank

render_click(Just (x,y)) =
  let
    txt = "(" <> approximate(x) <> "," <> approximate(y) <> ")"
  in
    translated(text("click"),7,9)
    & translated(text(txt),7,8)
    & translated(rectangle(4.5,2),7,8.5)

render_data(name,position,velocity,row) = blank
    & translated(text(name),-4,8-row)
    & translated(text(approximate(position)),-0.5,8-row)
    & translated(text(approximate(velocity)),2,8-row)


-------------------------------------------------------------------------------
--- Code for user interaction
-------------------------------------------------------------------------------

handle(model,event) =
    case event of
      MousePress(LeftButton,p) -> click_on(p)
      _ -> model
    where
    click_on(p) | within_graph(pchart') (p) =
      let
        (t',x') = to_graph(pchart')(p)
        values = [estimate t' (pgraph s) | s <- states(model)]
        p' = closest x' values
      in
        model { click = Just p' }
      
    click_on(p) | within_graph(vchart') (p) =
      let
        (t',x') = to_graph(vchart')(p)
        values = [estimate t' (vgraph s) | s <- states(model)]
        p' = closest x' values
      in
        model { click = Just p' }

    click_on(p) | within_sim(sim') (p) = model { click = Just (to_sim(sim')(p))}

    click_on(p) = model { click = Nothing }

    pchart' = pchart model
    vchart' = vchart model
    sim' = sim model

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- using_simtime(upd,stime)(model,dtime) = upd(model,stime)

-- using_simboost(upd,boost)(model,dtime) = upd(model,boost*dtime)

approximate(x) =
  let
    x' = rounded(10*x)
    insertdot(txt) =
      let
        chars1 = characters(txt)
        chars2 | 0 <= x', x' < 10 = "0" : chars1
               | 0 > x', x' > -10 = "-" : "0" : rest(chars1,1)
               | otherwise = chars1
        chars3 = reversed(chars2)
        chars4 = first(chars3,1) ++ ["."] ++ rest(chars3,1)
      in
        joined(reversed(chars4))
  in
    insertdot(printed(x'))
      
estimate :: Number -> [Point] -> Point
estimate t [p] = p
estimate t ((t1,x1):(t2,x2):pts) | t >= t1 = (t1,x1)
                                 | t1 > t, t >= t2 = (t2,x1)
                                 | otherwise = estimate t ((t2,x2):pts)

closest :: Number -> [Point] -> Point
closest x' pts =
  let
    closest' [(t,x)] = (abs(x'-x),(t,x))
    closest' ((t,x):pts') =
      let
        d_tx = abs(x'-x)
        (d_pts,tx_pts) = closest' pts'
      in
        if d_tx <= d_pts then (d_tx,(t,x))
        else (d_pts,tx_pts)
  in
    secondOfPair(closest' pts)

minilabel(num,s) = minitext(approximate(num),s)

minitext(txt,s) = scaled(text(txt),s,s)

