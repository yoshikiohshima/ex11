-module(morph).
-author(ohshima).
-export([newMorph/5]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-define(containsPoint(X, Y, W, H, MX, MY),
        (X =< MX) andalso (MX < (X + W)) andalso (Y =< MY) andalso (MY < (Y + H))).

-record(data, {x = 0, y = 0, width = 50, height = 40, color}).
-record(handler, {down = {none, {}}, up = {none, {}}, move = {none, {}}}).

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
    beDraggable ->
      loop(Morphic, Display, Pix, Data, Handler#handler{down={drag, {}}, up={drag, {}}, move={drag, {}}});
    {buttonPress, {P, BX, BY, _, _}} -> 
      io:format("press: ~p~n", [Handler]),
      {F, _} = Handler#handler.down,
      down(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Display, Pix, Data, Handler);
    {buttonMove, {P, BX, BY, _, _}} ->
      {F, _} = Handler#handler.move,
      move(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Display, Pix, Data, Handler);
    {buttonRelease, {P, BX, BY, _, _}} ->
      {F, _} = Handler#handler.up,
      up(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Display, Pix, Data, Handler);
    {draw, T} ->
      Morphic ! {'tell', {self(), T, Data}},
      draw(Morphic, Display, Pix, Data),
      loop(Morphic, Display, Pix, Data, Handler);
    _ -> loop(Morphic, Display, Pix, Data, Handler)
  end.

down(none, _, _, _, _) -> true;
down(drag, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  NewHandler = Handler#handler{down = {drag, {BX - Data#data.x, BY - Data#data.y}}},
  Morphic ! {'focus', self()},
  self() ! {'handlers', NewHandler}.

move(none, _, _, _, _) -> true;
move(drag, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  OrigT = Handler#handler.down,
  case OrigT of
    {_, {OrigXDiff, OrigYDiff}} ->
     self() ! {'data', Data#data{x = BX - OrigXDiff, y = BY - OrigYDiff}};
    _ -> true
  end.

up(none, _, _, _, _) -> true;
up(drag, Handler, EV, Data, Morphic) ->
  Morphic ! {'unfocus'},
  NewHandler = Handler#handler{down = {drag, {}}},
  self() ! {'handlers', NewHandler}.

draw(Morphic, Display, Pix, Data) -> 
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, Data#data.color)}]),
  Rect = mkRectangle(Data#data.x, Data#data.y, Data#data.width, Data#data.height),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
