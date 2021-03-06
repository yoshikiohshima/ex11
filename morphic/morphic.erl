-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1, xCreateGC/2, xColor/2, mkRectangle/4,
    xCreateSimpleWindow/7, eMapWindow/1, ePolyFillRectangle/3,
    xCreatePixmap/4, eCopyArea/9]).

-import(morph, [newMorph/6]).
-import(timer, [newTimer/1]).
-import(lasersocks, [startLaserSocks/0]).

-define(containsPoint(X, Y, W, H, MX, MY),
  (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  Win = xCreateSimpleWindow(Display, 400, 0, 800, 600, ?XC_arrow, xColor(Display,?gray)),
  Pix = xCreatePixmap(Display, Win, 800, 600),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  Nil = spawn(morph, newMorph, [self(), 0, 0, 0, 0, 16#000000]),
  M1 = spawn(morph, newMorph, [self(), 350, 0, 20, 20, 16#80D0D0]),
  M2 = spawn(morph, newMorph, [self(), 300, 0, 20, 20, 16#D080D0]),
  Scene = [M1, M2],
  Props = #{}, % {Pid => data}; created at every frame
  M1 ! {beNewButton, newMorph, []},
  M2 ! {beNewButton, recognize, [lasersocks]},

  Ms = [
              [50, 100, 50, 150, 16#8080D0],
              [160, 150, 30, 70, 16#8080D0],
              [300, 100, 50, 150, 16#8080D0],
              [220, 150, 30, 70, 16#8080D0],
              [180, 50, 50, 50, 16#8080D0]
       ],

  lists:foreach(fun(P) ->
    self() ! {newMorph, P}
   end, Ms),

  Timer = spawn(timer, newTimer, [self()]),

  StartTime = erlang:system_time(),
  loop(Display, Win, Pix, Scene, Props, Nil, Nil, StartTime, StartTime, 0).

loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts) ->
  receive
    {'focus', Pid} ->
      loop(Display, Win, Pix, Scene, Props, Pid, Nil, StartTime, LastRequestTime, Ts);
    {'unfocus'} ->
      loop(Display, Win, Pix, Scene, Props, Nil, Nil, StartTime, LastRequestTime, Ts);
    {event, _, buttonPress, E} ->
      {_, BX, BY, _, _} = E,
      target(Scene, BX, BY, Props, Nil) ! {'buttonPress', E},
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    {event, _, motionNotify, E} ->
      {_, BX, BY, _, _} = E,
      case Focus /= Nil of
        true -> Target = Focus;
        _    -> Target = target(Scene, BX, BY, Props, Nil)
      end,
      Target ! {'buttonMove', E},
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    {event, _, buttonRelease, E} ->
      {_, BX, BY, _, _} = E,
      target(Scene, BX, BY, Props, Nil) ! {'buttonRelease', E},
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    {'tell', {Pid, T, Data}} ->
      drawMorph(self(), Display, Pix, Data),
      NewTs = Ts - 1,
      case NewTs of
        0 -> copyPix(Display, Win, Pix),
             loop(Display, Win, Pix, Scene, Props#{Pid=>Data}, Focus, Nil, StartTime, 0, NewTs);
        _ -> 
             loop(Display, Win, Pix, Scene, Props#{Pid=>Data}, Focus, Nil, StartTime, LastRequestTime, NewTs)
        end;
    display ->
      case LastRequestTime of
        0 ->
         T = erlang:system_time() - StartTime,
         NewTs = length(Scene),
         draw(Display, Scene, Pix, T),
         loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, T, NewTs);
        _ ->
         %copyPix(Display, Win, Pix),
         loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, 0, Ts)
       end;
    copyPix ->
      copyPix(Display, Win, Pix),
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, 0, Ts);
    {newMorph, Param} ->
      case Param of
        [] ->
           RealParam = [175, 180, 50, 40, 16#8080D0];
        _  ->
           RealParam = Param
      end,
      M = spawn(morph, newMorph, [self() | RealParam]),
      M ! beDraggable,
      R = spawn(morph, newMorph, [self(), 225, 220, 10, 10, 16#D0D080]),
      R ! {'beResizer', M},
      loop(Display, Win, Pix, [M, R | Scene], Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    {addMorph, M} ->
      loop(Display, Win, Pix, [M | Scene], Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    {recognize, [R]} ->
      T = erlang:system_time() - StartTime,
      P = spawn(R, startLaserSocks, []),
      P ! {'recognize', Props, T, self()},
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts);
    X ->
      io:format("X: ~p~n", [X]),
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts)
  end.

drawMorph(Morphic, Display, Pix, Data) ->
  Color = xCreateGC(Display, [{function,'copy'},
                              {graphics_exposures, false},
                              {foreground, xColor(Display, maps:get(color, Data))}]),
  Rect = mkRectangle(maps:get(x, Data), maps:get(y, Data),
                     maps:get(width, Data), maps:get(height, Data)),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).

draw(Display, Scene, Pix, T) ->
  Back = xCreateGC(Display, [{function,'copy'},
 	 {graphics_exposures, true}, {foreground, xColor(Display, ?gray)}]),

  Rect = mkRectangle(0, 0, 800, 600),
  xDo(Display, ePolyFillRectangle(Pix, Back, [Rect])),
  lists:foreach(fun(M) -> M ! {ask, T, self()} end,
    lists:reverse(Scene)).

copyPix(Display, Win, Pix) ->
  Copy = xCreateGC(Display, [{function,'copy'},
                             {graphics_exposures, true},
			     {foreground, xColor(Display, ?white)}]),
  xDo(Display, eCopyArea(Pix, Win, Copy, 0, 0, 0, 0, 800, 600)),
  xFlush(Display).

target([], _, _, _, Nil) -> Nil;
target([M|MS], X, Y, Props, Nil) ->
  Prop = (catch maps:get(M, Props)),
  Val = is_map(Prop) andalso
            ?containsPoint(maps:get(x, Prop), maps:get(y, Prop), maps:get(width, Prop), maps:get(height, Prop), X, Y),
  case Val of
    true -> M;
    _ -> target(MS, X, Y, Props, Nil)
  end.
