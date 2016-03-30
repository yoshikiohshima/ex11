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
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, ?blue)}]),
  morph_loop(Display, Pix, W, H, X, Y, Color, {0, 0, 0}).   % {down, up, move}.  Values are either zero or a record of {fun, params}.

morph_loop(Display, Pix, W, H, X, Y, Color, E) ->
  {Down, Up, Move} = E,
  receive
    {beDraggable} ->
      NewX = X,
      NewY = Y,
      NewE = {{drag, {}}, {drag, {}}, {drag, {}}};
    {buttonPress, {P, BX, BY, SX, SY}}
         when ?containsPoint(X, Y, W, H, BX, BY),
              Down /= 0 -> 
      {F, Param} = Down,
      NewDown = down(F, E, {P, BX, BY, SX, SY}, X, Y),
      NewE = {NewDown, Up, Move},
      NewX = X,
      NewY = Y;
    {buttonMove, {P, BX, BY, SX, SY}}
         when ?containsPoint(X, Y, W, H, BX, BY),
              Move /= 0 -> 
      {F, Param} = Move,
      NewMove = move(F, E, {P, BX, BY, SX, SY}, X, Y),
      {_, {NewX, NewY}} = NewMove,
      NewE = {Down, Up, NewMove};
    {buttonUp, {P, BX, BY, SX, SY}}
         when ?containsPoint(X, Y, W, H, BX, BY),
              Up /= 0 -> 
      {F, Param} = Up,
      NewUp = up(F, E, {P, BX, BY, SX, SY}, X, Y),
      NewX = X,
      NewY = Y,
      NewE = {Down, NewUp, Move};
    {morph_draw} ->
      NewE = E,
      NewX = X,
      NewY = Y,
      draw(Display, Pix, W, H, X, Y, Color);
    _ -> 
      NewE = E,
      NewX = X,
      NewY = Y
  end,
  morph_loop(Display, Pix, W, H, NewX, NewY, Color, NewE).

down(drag, E, EV, X, Y) ->
  {P, BX, BY, SX, SY} = EV,
  {drag, {BX - X, BY - Y}}.

move(drag, E, EV, X, Y) ->
  {P, BX, BY, SX, SY} = EV,
  {{_, {OrigXDiff, OrigYDiff}}, _, _} = E,
  {drag, {BX - OrigXDiff, BY - OrigYDiff}}.

up(drag, E, EV, X, Y) ->
  {P, BX, BY, SX, SY} = EV,
  {{_, {OrigXDiff, OrigYDiff}}, _, _} = E,
  {drag, 0, 0}.

draw(Display, Pix, W, H, X, Y, Color) -> 
  Rect = mkRectangle(X, Y, W, H),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
