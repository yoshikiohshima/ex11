-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,ePolyFillRectangle/3,xCreatePixmap/4,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-import(morph, [newMorph/4]).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  xSetScreenSaver(Display,0),
  Win = xCreateSimpleWindow(Display,400,0,400,400,?XC_arrow,xColor(Display,?black)),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  M = spawn(morph, newMorph, [Display, Win, 300, 300]),
  N = spawn(morph, newMorph, [Display, Win, 250, 250]),
  Scene = [M, N],
  loop(Display, Win, Scene).

loop(Display, Win, Scene) ->
  receive
    {event, _, buttonPress, _} ->
      lists:nth(1, Scene) ! {'buttonPress'}
  after 1000 -> 
    io:format("after ~p~n", [Scene]),
    draw(Display, Win, Scene)
  end,
  loop(Display, Win, Scene).

draw(Display, Win, Scene) ->
    Black = xCreateGC(Display, [{function,'set'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, true},{foreground, xColor(Display, ?white)}]),

  Rect = mkRectangle(0, 0, 400, 200),
  xDo(Display, ePolyFillRectangle(Win, Black, [Rect])),
  xFlush(Display),
  lists:map(fun(M) -> 
    M ! {'morph_draw'} end,
    Scene),
  xFlush(Display).
