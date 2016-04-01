-module(morph).
-author(ohshima).
-export([newMorph/4]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

-record(morphData, {x = 0, y = 0, width = 50, height = 40, color}).
-record(handler, {down = 0, up = 0, move = 0}).

newMorph(Display, Pix, X, Y) ->
 init(Display, Pix, X, Y).

init(Display, Pix, X, Y) ->
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, 16#1010E0)}]),
  Data = #morphData{x=X, y=Y, color = Color},
  Handler = #handler{},
  morph_loop(Display, Pix, Data, Handler).   % {down, up, move}.  Values are either zero or a record of {fun, params}.

morph_loop(Display, Pix, Data, Handler) ->
  io:format("Handler: ~p~n", [Handler#handler.down]),
  receive
    {'data', NewData} ->
       morph_loop(Display, Pix, NewData, Handler);
    {'handlers', NewHandler} ->
      morph_loop(Display, Pix, Data, NewHandler);
    {beDraggable} ->
      self() ! {'handlers', Handler#handler{down={drag, {}}, up={drag, {}}, move={drag, {}}}},
      morph_loop(Display, Pix, Data, Handler);
    {buttonPress, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
               Handler#handler.down /= 0 -> 
      {F, _} = Handler#handler.down,
      down(F, Handler, {P, BX, BY}, Data),
      morph_loop(Display, Pix, Data, Handler);
    {buttonMove, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
              Handler#handler.move /= 0 -> 
      {F, _} = Handler#handler.move,
      move(F, Handler, {P, BX, BY}, Data),
      morph_loop(Display, Pix, Data, Handler);
    {buttonUp, {P, BX, BY, _, _}}
         when ?containsPoint(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height, BX, BY),
              Handler#handler.up /= 0 -> 
      {F, _} = Handler#handler.up,
      up(F, Handler, {P, BX, BY}, Data),
      morph_loop(Display, Pix, Data, Handler);
    {morph_draw} ->
      draw(Display, Pix, Data),
      morph_loop(Display, Pix, Data, Handler);
    _ -> morph_loop(Display, Pix, Data, Handler)
  end.
  

down(drag, Handler, EV, Data) ->
  {P, BX, BY} = EV,
  NewHandler = Handler#handler{down = {drag, {BX - Data#morphData.x, BY - Data#morphData.y}}},
  self() ! {'handlers', NewHandler}.

move(drag, Handler, EV, Data) ->
  {P, BX, BY} = EV,
  OrigT = Handler#handler.down,
  {_, {OrigXDiff, OrigYDiff}} = OrigT,
  self() ! {'data', Data#morphData{x = BX - OrigXDiff, y = BY - OrigYDiff}}.

up(drag, E, EV, Data) ->
  true.

draw(Display, Pix, Data) -> 
  Rect = mkRectangle(Data#morphData.x, Data#morphData.y, Data#morphData.width, Data#morphData.height),
  xDo(Display, ePolyFillRectangle(Pix, Data#morphData.color, [Rect])).
