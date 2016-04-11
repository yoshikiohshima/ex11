-module(morph).
-author(ohshima).
-export([newMorph/6]).
-include("ex11_lib.hrl").
-import(ex11_lib, [xDo/2, xCreateGC/2,xColor/2,mkRectangle/4, ePolyFillRectangle/3]).

-record(handler, {down = {none, {}}, up = {none, {}}, move = {none, {}}}).

newMorph(Morphic, X, Y, W, H, C) -> init(Morphic, X, Y, W, H, C).

init(Morphic, X, Y, W, H, C) ->
  Handler = #handler{}, % {down, up, move}.  Values are either zero or a record of {fun, params}.
  Data = #{x=>X, y=>Y, color=>C, width=>W, height=>H, type=>'none'}, % x, y, color, width, height and handler type
  loop(Morphic, Data, Handler).

loop(Morphic, Data, Handler) ->
  receive
    {'data', NewData} ->
      loop(Morphic, NewData, Handler);
    {'handlers', NewHandler} ->
      {Type, _} = NewHandler#handler.down,
      NewData = maps:put(type, Type, Data),
      loop(Morphic, NewData, NewHandler);
    beDraggable ->
      NewData = maps:put(type, 'drag', Data),
      loop(Morphic, NewData,
           Handler#handler{down={drag, {}}, up={drag, {}}, move={drag, {}}});
    {beNewButton, N, List} ->
      NewData = maps:put(type, 'button', Data),
      loop(Morphic, NewData,
           Handler#handler{down={button, {N, List}}, up={button, {}}, move={button, {}}});
    {beResizer, Target} ->
      NewData = maps:put(type, 'resize', Data),
      loop(Morphic, NewData,
           Handler#handler{down={resize, {}}, up={resize, {}}, move={resize, {Target, 0, 0}}});
    {buttonPress, {P, BX, BY, _, _}} ->
      F = maps:get(type, Data),
      down(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {buttonMove, {P, BX, BY, _, _}} ->
      F = maps:get(type, Data),
      move(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {buttonRelease, {P, BX, BY, _, _}} ->
      F = maps:get(type, Data),
      up(F, Handler, {P, BX, BY}, Data, Morphic),
      loop(Morphic, Data, Handler);
    {draw, T, Morphic, Display, Pix} ->
      Morphic ! {'tell', {self(), T, Data}},
      draw(Morphic, Display, Pix, Data),
      loop(Morphic, Data, Handler);
    {resizeBy, {DW, DH}} ->
      NewData =    maps:put(width, maps:get(width, Data) + DW, Data),
      NewNewData = maps:put(height, maps:get(height, Data) + DH, NewData),
      loop(Morphic, NewNewData, Handler);
    {resizeTo, {W, H}} ->
      NewData =    maps:put(width, W, Data),
      NewNewData = maps:put(height, H, NewData),
      loop(Morphic, NewNewData, Handler);
    _ -> loop(Morphic, Data, Handler)
  end.

down(none, _, _, _, _) -> true;
down(drag, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  NewHandler = Handler#handler{down = {drag, {BX - maps:get(x, Data), BY - maps:get(y, Data)}}},
  Morphic ! {'focus', self()},
  self() ! {'handlers', NewHandler};
down(button, Handler, EV, _, Morphic) ->
  {_, Action} = Handler#handler.down,
%  {_, BX, BY} = EV,
  Morphic ! Action;
down(resize, Handler, EV, Data, Morphic) ->
  {_, BX, BY} = EV,
  {_, {Target, _, _}} = Handler#handler.move,
  NewHandler = Handler#handler{down = {resize, {BX, BY, maps:get(x, Data), maps:get(y, Data)}}, move = {resize, {Target, BX, BY}}},
  Morphic ! {'focus', self()},
  self() ! {'handlers', NewHandler};
down(remote, Handler, EV, Data, Morphic) ->
  {_, Recipient} = Handler#handler.down,
  Recipient ! {'buttonPress', self()}.

move(none, _, _, _, _) -> true;
move(drag, Handler, EV, Data, _) ->
  {_, BX, BY} = EV,
  OrigT = Handler#handler.down,
  case OrigT of
    {_, {OrigX, OrigY}} ->
      NewData = maps:put(y, (BY - OrigY), Data),
      NewNewData = maps:put(x, (BX - OrigX), NewData),
      self() ! {'data', NewNewData};
    _ -> true
  end;
move(button, _, _, _, _) ->
  true;
move(resize, Handler, EV, Data, _) ->
  {_, BX, BY} = EV,
  MaybeTarget = Handler#handler.move,
  case MaybeTarget of
    {_, {Target, PrevBX, PrevBY}} ->
      OrigT = Handler#handler.down,
      case OrigT of
        {_, {OrigBX, OrigBY, OrigX, OrigY}} ->
          NewData = maps:put(y, OrigY + (BY - OrigBY), Data),
          NewNewData = maps:put(x, OrigX + (BX - OrigBX), NewData),
          self() ! {'data', NewNewData},
          NewHandler = Handler#handler{move = {resize, {Target, BX, BY}}},
          self() ! {'handlers', NewHandler},
          Target ! {'resizeBy', {BX - PrevBX, BY - PrevBY}};
        _ -> true
      end;
    _ -> true
  end;
move(remote, Handler, EV, Data, Morphic) ->
  {_, Recipient} = Handler#handler.move,
  Recipient ! {'buttonMove', self()}.

up(none, _, _, _, _) -> true;
up(drag, Handler, EV, Data, Morphic) ->
  Morphic ! {'unfocus'},
  NewHandler = Handler#handler{down = {drag, {}}},
  self() ! {'handlers', NewHandler};
up(button, _, _, _, _) -> true;
up(resize, Handler, EV, Data, Morphic) ->
  Morphic ! {'unfocus'},
  NewHandler = Handler#handler{down = {resize, {}}},
  self() ! {'handlers', NewHandler};
up(remote, Handler, EV, Data, Morphic) ->
  {_, Recipient} = Handler#handler.up,
  Recipient ! {'buttonRelease', self()}.

draw(Morphic, Display, Pix, Data) -> 
  Color = xCreateGC(Display, [{function,'copy'}, {graphics_exposures, false},{foreground, xColor(Display, maps:get(color, Data))}]),
  Rect = mkRectangle(maps:get(x, Data), maps:get(y, Data), maps:get(width, Data), maps:get(height, Data)),
  xDo(Display, ePolyFillRectangle(Pix, Color, [Rect])).
