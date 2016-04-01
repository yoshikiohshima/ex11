-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,ePolyFillRectangle/3,xCreatePixmap/4,	 eCopyArea/9,
    ePutImage/9,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-import(morph, [newMorph/5]).
-record(data, {x, y, width, height, color}).

-define(containsPoint(X, Y, W, H, MX, MY),
  (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  xSetScreenSaver(Display,0),
  Win = xCreateSimpleWindow(Display,400,0,400,400,?XC_arrow,xColor(Display,?gray)),
  Pix = xCreatePixmap(Display, Win, 400, 400),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  M = spawn(morph, newMorph, [self(), Display, Pix, 300, 300]),
  N = spawn(morph, newMorph, [self(), Display, Pix, 250, 250]),
  Scene = [M, N],
  Props = #{}, % {Pid => data}; created at every frame
  M ! {'beDraggable'},
  loop(Display, Win, Pix, Scene, Props).

loop(Display, Win, Pix, Scene, Props) ->
  io:format("Props: ~p~n", [Props]),
  receive
    {'newMorph', Pid} ->
      loop(Display, Win, Pix, [Pid | Scene], Props);
    {event, _, buttonPress, E} ->
      lists:nth(1, Scene) ! {'buttonPress', E},
      loop(Display, Win, Pix, Scene, Props);
    {event, _, motionNotify, E} ->
      lists:nth(1, Scene) ! {'buttonMove', E},
      loop(Display, Win, Pix, Scene, Props);
    {event, _, motionRelease, E} ->
      lists:nth(1, Scene) ! {'buttonUp', E},
      loop(Display, Win, Pix, Scene, Props);
    {copyPix} ->
      copyPix(Display, Win, Pix),
      loop(Display, Win, Pix, Scene, Props);
    {'tell', {Pid, Data}} ->
      loop(Display, Win, Pix, Scene, Props#{Pid=>Data});
    X ->
      io:format("X: ~p~n", [X]),
      loop(Display, Win, Pix, Scene, Props)
  after 20 ->
    draw(Display, Win, Scene, Pix),
    loop(Display, Win, Pix, Scene, #{})
  end.

draw(Display, Win, Scene, Pix) ->
  Back = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, true},{foreground, xColor(Display, ?gray)}]),

  Rect = mkRectangle(0, 0, 400, 400),
  xDo(Display, ePolyFillRectangle(Pix, Back, [Rect])),
%  xFlush(Display),
  lists:foreach(fun(M) -> 
    M ! {'draw'} end,
    Scene),
  self() ! {'copyPix'}.

copyPix(Display, Win, Pix) ->
  Copy = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, true}, {foreground, xColor(Display, ?white)}]),

  xDo(Display, eCopyArea(Pix, Win, Copy, 0, 0, 0, 0, 400, 400)),
  xFlush(Display).

target([], X, Y, Props) -> false;
target([M|MS], X, Y, Props) ->
  Prop = (catch maps:get(M, Props)),
  Val = is_record(Prop, data) andalso ?containsPoint(Prop#data.x, Prop#data.y, Prop#data.width, Prop#data.height, X, Y),
  io:format("Val: ~p~n", [Val]),
  case Val of
    true -> M;
    _ -> target(MS, X, Y, Props)
  end.
