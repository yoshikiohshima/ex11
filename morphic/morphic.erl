-module(morphic).
-author(ohshima).
-export([start/0,init/0]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,ePolyFillRectangle/3,xCreatePixmap/4,	 eCopyArea/9,
    ePutImage/9,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-import(morph, [newMorph/4]).

start() -> spawn(?MODULE,init,[]).

init() -> 
  {ok, Display} = ex11_lib:xStart("3.1"),
  xSetScreenSaver(Display,0),
  Win = xCreateSimpleWindow(Display,400,0,400,400,?XC_arrow,xColor(Display,?black)),
  Pix = xCreatePixmap(Display, Win, 400, 400),
  xDo(Display, eMapWindow(Win)),
  xFlush(Display),
  M = spawn(morph, newMorph, [Display, Pix, 300, 300]),
  N = spawn(morph, newMorph, [Display, Pix, 250, 250]),
  Scene = [M, N],
  loop(Display, Win, Scene, Pix).

loop(Display, Win, Scene, Pix) ->
  receive
    {event, _, buttonPress, _} ->
      lists:nth(1, Scene) ! {'buttonPress'}
  after 1000 -> 
    io:format("after ~p~n", [Scene]),
    draw(Display, Win, Scene, Pix)
  end,
  loop(Display, Win, Scene, Pix).

draw(Display, Win, Scene, Pix) ->
    Black = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, true},{foreground, xColor(Display, ?black)}]),

  Rect = mkRectangle(0, 0, 400, 400),
  xDo(Display, ePolyFillRectangle(Pix, Black, [Rect])),
  xFlush(Display),
  lists:map(fun(M) -> 
    M ! {'morph_draw'} end,
    Scene),

  Copy = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
 	 {graphics_exposures, true}, {foreground, xColor(Display, ?white)}]),

  xDo(Display, eCopyArea(Pix, Win, Copy, 0, 0, 0, 0, 400, 400)),
%  ePutImage(Win, Copy, 400, 400, 0, 0, 0, 32, Pix),
  xFlush(Display).

