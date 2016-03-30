-module(morph).
-author(ohshima).
-export([newMorph/4]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

-record(morphData, {x = 0, y = 0, width = 50, height = 40, color}).
-record(handlerData, {down = 0, up = 0, move = 0}).

newMorph(Display, Pix, X, Y) ->
 init(Display, Pix, X, Y).

init(Display, Pix, X, Y) ->
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, 16#1010E0)}]),
  Data = #morphData{x=X, y=Y, color = Color},
  morph_loop(Display, Pix, Data, {0, 0, 0}).   % {down, up, move}.  Values are either zero or a record of {fun, params}.

morph_loop(Display, Pix, Data, E) ->
  {Down, Up, Move} = E,
  receive
    {beDraggable} ->
      NewE = {{drag, {}}, {drag, {}}, {drag, {}}};
    {'data', NewData} ->
       NewE = E,
       morph_loop(Display, Pix, NewData, E);
    {buttonPress, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
              Down /= 0 -> 
      {F, Param} = Down,
      NewDown = down(F, E, {P, BX, BY}, Data),
      NewE = {NewDown, Up, Move};
    {buttonMove, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
              Move /= 0 -> 
      {F, Param} = Move,
      NewMove = move(F, E, {P, BX, BY}, Data),
      NewE = {Down, Up, NewMove};
    {buttonUp, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
              Up /= 0 -> 
      {F, Param} = Up,
      NewUp = up(F, E, {P, BX, BY}, Data),
      NewE = {Down, NewUp, Move};
    {morph_draw} ->
      NewE = E,
      draw(Display, Pix, Data);
    _ -> 
      NewE = E
  end,
  morph_loop(Display, Pix, Data, NewE).

down(drag, E, EV, Data) ->
  {P, BX, BY} = EV,
  {drag, {BX - Data#morphData.x, BY - Data#morphData.y}}.

move(drag, E, EV, Data) ->
  {P, BX, BY} = EV,
  {{_, {OrigXDiff, OrigYDiff}}, _, _} = E,
  self() ! {'data', Data#morphData{x = BX - OrigXDiff, y = BY - OrigYDiff}},
  {drag, {}}.

up(drag, E, EV, Data) ->
%  {P, BX, BY} = EV,
  {{_, {OrigXDiff, OrigYDiff}}, _, _} = E,
  {drag, {}}.

draw(Display, Pix, Data) -> 
  Rect = mkRectangle(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height),
  xDo(Display, ePolyFillRectangle(Pix, Data#morphData.color, [Rect])).
