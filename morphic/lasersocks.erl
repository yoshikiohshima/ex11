-module(lasersocks).
-author(ohshima).
-export([startLaserSocks/0]).

-import(morph, [newMorph/6]).
-record(handler, {down = {none, {}}, up = {none, {}}, move = {none, {}}}).

startLaserSocks() -> loop(#{}, false, #{}).

compare({_, A}, {_, B}) ->
  AX = maps:get(x, A),
  AY = maps:get(x, B),
  AX =< AY.

isUserWidget(A) ->
  {_, Data} = A,
  case maps:get(type, Data) of
    drag -> true;
    _    -> false
  end.

gameStart([{Player1, Player1Data}, 
           {Meter1, Meter1Data},
           {StartButton, StartButtonData},
           {Meter2, Meter2Data},
           {Player2, Player2Data}], Morphic) ->
  P1 = spawn(morph, newMorph,
             [Morphic,
              maps:get(x, Player1Data) + 3,
              maps:get(y, Player1Data) + 3,
              maps:get(width, Player1Data),
              maps:get(height, Player1Data),
              16#D0D0D0]),
  Morphic ! {'addMorph', P1},
  P2 = spawn(morph, newMorph,
             [Morphic,
              maps:get(x, Player2Data) + 3,
              maps:get(y, Player2Data) + 3,
              maps:get(width, Player2Data),
              maps:get(height, Player2Data),
              16#D0D0D0]),
  Morphic ! {'addMorph', P2},
  M1 = spawn(morph, newMorph,
             [Morphic,
              maps:get(x, Meter1Data) + 3,
              maps:get(y, Meter1Data) + 3,
              maps:get(width, Meter1Data),
              maps:get(height, Meter1Data),
              16#D0D0D0]),
  Morphic ! {'addMorph', M1},
  M2 = spawn(morph, newMorph,
             [Morphic,
              maps:get(x, Meter2Data) + 3,
              maps:get(y, Meter2Data) + 3,
              maps:get(width, Meter2Data),
              maps:get(height, Meter2Data),
              16#D0D0D0]),
  Morphic ! {'addMorph', M2},
  S = spawn(morph, newMorph,
             [Morphic,
              maps:get(x, StartButtonData) + 3,
              maps:get(y, StartButtonData) + 3,
              maps:get(width, StartButtonData),
              maps:get(height, StartButtonData),
              16#D0D0D0]),
  Morphic ! {'addMorph', S},

  Player1 ! {'handlers', #handler{down = {remote, self()}, move = {remote, self()}, up = {remote, self()}}},
  Player2 ! {'handlers', #handler{down = {remote, self()}, move = {remote, self()}, up = {remote, self()}}},

  {Player1, Meter1, StartButton, Meter2, Player2,
   P1, M1, S, M2, P2, Meter1Data, Meter2Data}.

loop(Widgets, GameStarted, State) ->
  receive
    {'recognize', Props, T, Morphic} ->
      List = maps:to_list(Props),
      Filtered = lists:filter(fun(A) -> isUserWidget(A) end, List),
      case length(Filtered) of
        5 -> 
          Sorted = lists:sort(fun(A, B) -> compare(A, B) end, Filtered),
          NewWidgets = gameStart(Sorted, Morphic);
        _ ->
          NewWidgets = Widgets
      end,
      loop(NewWidgets, GameStarted, State);
    {'buttonPress', M} ->
      {Player1, Meter1, StartButton, Meter2, Player2,
       P1, M1, S, M2, P2, M1Data, M2Data} = Widgets,
      io:format("button ~p, ~p~n", [M, Player1]),
      case M of
        Player1
          ->
           io:format("is player1~n", []),
           M1 ! {'resizeBy', {0, 2}};
        Player2
          -> M2 ! {'resizeBy', {0, 2}};
        StartButton ->
          W1 = maps:get(width, M1Data),
          M1 ! {'resizeTo', {W1, 2}},
          W2 = maps:get(width, M2Data),
          M2 ! {'resizeTo', {W2, 2}};
        _ -> true
      end;
    X ->
      io:format("X: ~p~n", [X]),
      loop(Widgets, GameStarted, State)
  end.
