-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,ePolyFillRectangle/3,xCreatePixmap/4,eCopyArea/9,ePolyArc/3,ePolyFillArc/3,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-import(morph, [newMorph/2, test/0]).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  xSetScreenSaver(Display,0),
  Win = xCreateSimpleWindow(Display,400,0,400,400,?XC_arrow,xColor(Display,?black)),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  M = spawn(morph, newMorph, [Display,Win]),
  io:format("morph: ~p, ~p~n", [Display, M]),
  Scene = [M],
  loop(Display, Win, Scene).

loop(Display, Win, Scene) ->
  receive
    Msg ->
      io:format("~p got msg: ~p~n",[?MODULE, Msg])
  after 1000 -> 
    io:format("after ~p~n", [Scene]),
    draw(Display, Win, Scene)
  end,
  loop(Display, Win, Scene).

draw(Display, Win, Scene) ->
    White = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, false},{foreground, xColor(Display, ?white)}]),

  Rect = mkRectangle(0, 0, 200, 200),
  io:format("Top: ~p, ~p, ~p~n",[Display, Rect, White]),
  xDo(Display, ePolyFillRectangle(Win, White, [Rect])),
  xFlush(Display),
  lists:map(fun(M) -> 
    M ! {'morph_draw'} end,
    Scene),
  xFlush(Display).
