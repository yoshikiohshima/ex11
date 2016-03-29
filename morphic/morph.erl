-module(morph).
-author(ohshima).
-export([newMorph/4]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,xCreateWindow/10,xCreateCursor/2,ePolyFillRectangle/3,xCreatePixmap/4,eCopyArea/9,ePolyArc/3,ePolyFillArc/3,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

newMorph(Display, Pix, X, Y) ->
 init(Display, Pix, X, Y).

init(Display, Pix, X, Y) ->
  W = 50,
  H = 40,
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, ?red)}]),
  morph_loop(Display, Pix, W, H, X, Y, Color).

morph_loop(Display, Pix, W, H, X, Y, Color) ->
  receive
    Msg ->
      io:format("~p got msg: ~p~n",[?MODULE, Msg])
  end,
  case Msg of 
    {buttonPress, {B, BX, BY, SX, SY}} when ?containsPoint(X, Y, W, H, BX, BY) -> 
      NewX = X + 5;
    {morph_draw} ->
      NewX = X,
      io:format("morph_draw~n"),
      draw(Display, Pix, W, H, NewX, Y, Color);
    _ -> 
      NewX = X,
      true
  end,
  morph_loop(Display, Pix, W, H, NewX, Y, Color).

containsPoint(X, Y, W, H, MX, MY) -> 
  (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H)).

draw(Display, Pix, W, H, X, Y, Color) -> 
  Rect = mkRectangle(X, Y, W, H),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
