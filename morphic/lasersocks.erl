-module(lasersocks).
-author(ohshima).
-export([startLaserSocks/0]).

startLaserSocks() -> loop(#{}, false).

compare({_, A}, {_, B}) ->
  AX = maps:get(x, A),
  AY = maps:get(x, B),
  AX =< AY.

getFirst([H | T]) ->
  H.

loop(Widgets, GameStarted) ->
  receive
    {'recognize', Props, T, Morphic} ->
      List = maps:to_list(Props),
      case length(List) of
        5 -> 
          Sorted = lists:sort(fun(A, B) -> compare(A, B) end, List),
          Morphic ! {'recognized', T, self(), lists:map(fun(A) -> {X, Y} = A, X end, Sorted)};
        _ -> true
      end,
      loop(Widgets, GameStarted);
    {'game', {Player1, Meter1, StartButton, Meter2, Player2}} ->
       loop(#{player1 => Player1, player2 => Player2, meter1 => Meter1, meter2 => Meter2, startButton => StartButton}, true)
  end.
