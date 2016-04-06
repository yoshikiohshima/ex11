-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1, xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,ePolyFillRectangle/3, xCreatePixmap/4, eCopyArea/9]).

-import(morph, [newMorph/3]).
-import(timer, [newTimer/1]).
-record(data, {x, y, width, height, color}).

-define(containsPoint(X, Y, W, H, MX, MY),
  (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  Win = xCreateSimpleWindow(Display, 400, 0, 400, 400, ?XC_arrow, xColor(Display,?gray)),
  Pix = xCreatePixmap(Display, Win, 400, 400),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  M = spawn(morph, newMorph, [self(), 300, 300]),
  N = spawn(morph, newMorph, [self(), 350, 0]),
  Nil = spawn(morph, newMorph, [self(), 0, 0]),
  Scene = [M, N],
  Props = #{}, % {Pid => data}; created at every frame
  M ! beDraggable,
  N ! beNewButton,

  Timer = spawn(timer, newTimer, [self()]),

  StartTime = erlang:system_time(),
  loop(Display, Win, Pix, Scene, Props, Nil, Nil, StartTime, StartTime, 0).

loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts) ->
  receive
    {'newMorph', Pid} ->
      loop(Display, Win, Pix, [Pid | Scene], Props, Focus, Nil, StartTime, LastRequestTime, Ts);
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
         copyPix(Display, Win, Pix),
         loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, 0, 0)
       end;
    copyPix ->
      copyPix(Display, Win, Pix),
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, 0, Ts);
    X ->
      io:format("X: ~p~n", [X]),
      loop(Display, Win, Pix, Scene, Props, Focus, Nil, StartTime, LastRequestTime, Ts)
  end.

draw(Display, Scene, Pix, T) ->
  Back = xCreateGC(Display, [{function,'copy'},
 	 {graphics_exposures, true}, {foreground, xColor(Display, ?gray)}]),

  Rect = mkRectangle(0, 0, 400, 400),
  xDo(Display, ePolyFillRectangle(Pix, Back, [Rect])),
  lists:foreach(fun(M) -> 
    M ! {'draw', T, self(), Display, Pix} end,
    Scene).

copyPix(Display, Win, Pix) ->
  Copy = xCreateGC(Display, [{function,'copy'},
                             {graphics_exposures, true},
			     {foreground, xColor(Display, ?white)}]),
  xDo(Display, eCopyArea(Pix, Win, Copy, 0, 0, 0, 0, 400, 400)),
  xFlush(Display).

target([], _, _, _, Nil) -> Nil;
target([M|MS], X, Y, Props, Nil) ->
  Prop = (catch maps:get(M, Props)),
  Val = is_record(Prop, data) andalso
            ?containsPoint(Prop#data.x, Prop#data.y, Prop#data.width, Prop#data.height, X, Y),
  case Val of
    true -> M;
    _ -> target(MS, X, Y, Props, Nil)
  end.
