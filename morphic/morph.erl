-module(morph).
-author(ohshima).
-export([newMorph/5]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

-record(data, {x = 0, y = 0, width = 50, height = 40, color}).
-record(handler, {down = 0, up = 0, move = 0}).

newMorph(Morphic, Display, Pix, X, Y) -> init(Morphic, Display, Pix, X, Y).

init(Morphic, Display, Pix, X, Y) ->
  Color = 16#1010E0,
  Data = #data{x=X, y=Y, color=Color},
  Handler = #handler{},
  loop(Morphic, Display, Pix, Data, Handler).   % {down, up, move}.  Values are either zero or a record of {fun, params}.

loop(Morphic, Display, Pix, Data, Handler) ->
  receive
    {'data', NewData} ->
       loop(Morphic, Display, Pix, NewData, Handler);
    {'handlers', NewHandler} ->
      loop(Morphic, Display, Pix, Data, NewHandler);
    {beDraggable} ->
      self() ! {'handlers', Handler#handler{down={drag, {}}, up={drag, {}}, move={drag, {}}}},
      loop(Morphic, Display, Pix, Data, Handler);
    {buttonPress, {P, BX, BY, _, _}}
         when ?containsPoint(Data#data.x, Data#data.y, Data#data.width, Data#data.height, BX, BY),
               Handler#handler.down /= 0 -> 
      {F, _} = Handler#handler.down,
      down(F, Handler, {P, BX, BY}, Data),
      loop(Morphic, Display, Pix, Data, Handler);
    {buttonMove, {P, BX, BY, _, _}}
         when ?containsPoint(Data#data.x, Data#data.y, Data#data.width, Data#data.height, BX, BY),
              Handler#handler.move /= 0 -> 
      {F, _} = Handler#handler.move,
      move(F, Handler, {P, BX, BY}, Data),
      loop(Morphic, Display, Pix, Data, Handler);
    {buttonUp, {P, BX, BY, _, _}}
         when ?containsPoint(Data#data.x, Data#data.y, Data#data.width, Data#data.height, BX, BY),
              Handler#handler.up /= 0 -> 
      {F, _} = Handler#handler.up,
      up(F, Handler, {P, BX, BY}, Data),
      loop(Morphic, Display, Pix, Data, Handler);
    {draw} ->
      Morphic ! {'tell', {self(), Data}},
      draw(Morphic, Display, Pix, Data),
      loop(Morphic, Display, Pix, Data, Handler);
    _ -> loop(Morphic, Display, Pix, Data, Handler)
  end.

down(drag, Handler, EV, Data) ->
  {_, BX, BY} = EV,
  NewHandler = Handler#handler{down = {drag, {BX - Data#data.x, BY - Data#data.y}}},
  self() ! {'handlers', NewHandler}.

move(drag, Handler, EV, Data) ->
  {_, BX, BY} = EV,
  OrigT = Handler#handler.down,
  {_, {OrigXDiff, OrigYDiff}} = OrigT,
  self() ! {'data', Data#data{x = BX - OrigXDiff, y = BY - OrigYDiff}}.

up(drag, Handler, EV, Data) ->
  true.

draw(Morphic, Display, Pix, Data) -> 
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, Data#data.color)}]),
  Rect = mkRectangle(Data#data.x, Data#data.y, Data#data.width, Data#data.height),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
