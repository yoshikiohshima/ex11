-module(morph).
-author(ohshima).
-export([newMorph/2]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,xCreateWindow/10,xCreateCursor/2,ePolyFillRectangle/3,xCreatePixmap/4,eCopyArea/9,ePolyArc/3,ePolyFillArc/3,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

newMorph(Display, Parent) ->
 init(Display, Parent).

init(Display, Parent) ->
  Width = 50,
  Height = 40,
  X = 100,
  Y = 100,
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, ?red)}]),
  morph_loop(Display, Parent, Width, Height, X, Y, Color).

morph_loop(Display, Parent, Width, Height, X, Y, Color) ->
  receive
    Msg ->
      io:format("~p got msg: ~p~n",[?MODULE, Msg])
  end,
  case Msg of 
    {'event',_,'buttonPress',_} -> 
      NewX = X + 1;
    {'morph_draw'} -> 
      NewX = X,
      io:format("morph_draw", []),
      draw(Display, Parent, Width, Height, NewX, Y, Color);
    _ -> 
      NewX = X,
      true
  end,
  morph_loop(Display, Parent, Width, Height, NewX, Y, Color).

draw(Display, Win, Width, Height, X, Y, Color) -> 
  Rect = mkRectangle(X, Y, Width, Height),
  io:format("Morph: ~p, ~p, ~p, ~p~n",[Display, Win, Rect, Color]),
  xDo(Display, ePolyFillRectangle(Win, Color, [Rect])).
