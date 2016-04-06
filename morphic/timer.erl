-module(timer).
-author(ohshima).
-export([newTimer/1]).

newTimer(Morphic) ->  loop(Morphic).

loop(Morphic) ->
  receive after 30 -> Morphic ! display end,
  loop(Morphic).
