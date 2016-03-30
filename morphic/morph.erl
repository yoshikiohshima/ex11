-module(morph).
-author(ohshima).
-export([newMorph/4]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xFlush/1,rpc/2,xCreateGC/2,xColor/2,mkRectangle/4,xCreateSimpleWindow/7,
    eMapWindow/1,xCreateWindow/10,xCreateCursor/2,ePolyFillRectangle/3,xCreatePixmap/4,eCopyArea/9,ePolyArc/3,ePolyFillArc/3,
    mkArc/6,mkPoint/2,ePolyLine/4,xPen/3,xSetScreenSaver/2]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

-define(has(K, M),
        case (catch maps:get(K, M)) of
           {'EXIT', _} -> false;
            _ -> true
        end).

newMorph(Display, Pix, X, Y) ->
 init(Display, Pix, X, Y).

init(Display, Pix, X, Y) ->
  W = 50,
  H = 40,
  B = #{},
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, ?red)}]),
  morph_loop(Display, Pix, W, H, X, Y, B, Color, {0, 0, 0}).   % {down, up, move}.  Values are either zero or a record of {fun, params}.

morph_loop(Display, Pix, W, H, X, Y, B, Color, E) ->
  io:format("E: ~p~n", [E]),
  {Down, Up, Move} = E,
  receive
    Msg ->
      io:format("~p got msg: ~p~n",[?MODULE, Msg])
  end,
  case Msg of
    {beDraggable} ->
      NewE = {{dragDown, {}}, {dragUp, {}}, {dragMove, {}}};
    {buttonPress, {P, BX, BY, SX, SY}}
         when ?containsPoint(X, Y, W, H, BX, BY),
              Down /= 0 -> 
      {F, Param} = Down,
      NewDown = down(F, {P, BX, BY, SX, SY}, X, Y),
      NewE = {NewDown, Up, Move};
    {morph_draw} ->
      NewE = E,
      draw(Display, Pix, W, H, X, Y, Color);
    _ -> 
      NewE = E
  end,
  morph_loop(Display, Pix, W, H, X, Y, B, Color, NewE).

down(dragDown, EV, X, Y) ->
  {P, BX, BY, SX, SY} = EV,
  {dragDown, {X, Y, BX, BY}}.


containsPoint(X, Y, W, H, MX, MY) -> 
  (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H)).

draw(Display, Pix, W, H, X, Y, Color) -> 
  Rect = mkRectangle(X, Y, W, H),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
