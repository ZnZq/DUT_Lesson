-module(sort).
-author("ZnZ").

-import(list, [concatenate/1]).
-export([fast/1]).

fast([]) -> [];
fast([H | T]) ->
  Left = fast([X || X <- T, X > H]),
  Right = fast([X || X <- T, X < H]),
  list:concatenate([Right, [H], Left]).