-module(morph).
-author(ohshima).
-export([newMorph/3]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-record(data, {x = 0, y = 0, width = 50, height = 40, color}).
-record(handler, {down = {none, {}}, up = {none, {}}, move = {none, {}}}).

newMorph(Morphic, X, Y) -> init(Morphic, X, Y).

init(Morphic, X, Y) ->
  Color = 16#1010E0,
  Data = #data{x=X, y=Y, color=Color},
  Handler = #handler{},
  % {down, up, move}.  Values are either zero or a record of {fun, params}.
  loop(Morphic, Data, Handler).

loop(Morphic, Data, Handler) ->
  receive
    {'data', NewData} ->
       loop(Morphic, NewData, Handler);
    {'handlers', NewHandler} ->
      loop(Morphic, Data, NewHandler);
    beDraggable ->
      loop(Morphic, Data,
           Handler#handler{down={drag, {}}, up={drag, {}}, move={drag, {}}});
    beNewButton ->
      loop(Morphic, Data,
           Handler#handler{down={button, {}}, up={button, {}}, move={button, {}}});
    {buttonPress, {P, BX, BY, _, _}} -> 
      {F, _} = Handler#handler.down,
      down(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {buttonMove, {P, BX, BY, _, _}} ->
      {F, _} = Handler#handler.move,
      move(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {buttonRelease, {P, BX, BY, _, _}} ->
      {F, _} = Handler#handler.up,
      up(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {draw, T, Morphic, Display, Pix} ->
      Morphic ! {'tell', {self(), T, Data}},
      draw(Morphic, Display, Pix, Data),
      loop(Morphic, Data, Handler);
    _ -> loop(Morphic, Data, Handler)
  end.

down(none, _, _, _, _) -> true;
down(drag, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  NewHandler = Handler#handler{down = {drag, {BX - Data#data.x, BY - Data#data.y}}},
  Morphic ! {'focus', self()},
  self() ! {'handlers', NewHandler};
down(button, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  M = spawn(morph, newMorph, [Morphic, 175, 180]),
  M ! beDraggable,
  Morphic ! {'newMorph', M}.

move(none, _, _, _, _) -> true;
move(drag, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  OrigT = Handler#handler.down,
  case OrigT of
    {_, {OrigXDiff, OrigYDiff}} ->
     self() ! {'data', Data#data{x = BX - OrigXDiff, y = BY - OrigYDiff}};
    _ -> true
  end;
move(button, _, _, _, _) ->
  true.

up(none, _, _, _, _) -> true;
up(drag, Handler, EV, Data, Morphic) ->
  Morphic ! {'unfocus'},
  NewHandler = Handler#handler{down = {drag, {}}},
  self() ! {'handlers', NewHandler};
up(button, _, _, _, _) -> true.

draw(Morphic, Display, Pix, Data) -> 
  Color = xCreateGC(Display, [{function,'copy'},{line_width,5},{arc_mode,chord},{line_style,solid},
                {graphics_exposures, false},{foreground, xColor(Display, Data#data.color)}]),
  Rect = mkRectangle(Data#data.x, Data#data.y, Data#data.width, Data#data.height),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
