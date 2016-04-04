-module(display).
-author(ohshima).
-export([newDisplay/1]).

newDisplay(Morphic) -> 
  loop(Morphic).

loop(Morphic) ->
  receive
    _ -> true
  after 30 ->
    Morphic ! {'display'}
  end,
  loop(Morphic).
