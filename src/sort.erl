-module(sort).
-author("ZnZ").

-import(list, [concatenate/1]).
-import(arr_r, [len/1]).
-export([fast/1, merge/1, split/1]).

fast([]) -> [];
fast([H | T]) ->
  Left = fast([X || X <- T, X < H]),
  Right = fast([X || X <- T, X > H]),
  list:concatenate([Left, [H], Right]).

merge([_] = L) -> L;
merge(List) ->
  {Left, Right} = split(List),
  L = merge(Left),
  R = merge(Right),
  q.

split(List) ->
  Center = round(arr_r:len(List) / 2),
  Left =  split(List, fun(I) -> I =< Center end, 1),
  Right = split(List, fun(I) -> I > Center end, 1),
  {Left, Right}.

split([], _, _) -> [];
split([H | T], Filter, Index) ->
  case Filter(Index) of
    true -> [H | split(T, Filter, Index + 1)];
    _ -> split(T, Filter, Index + 1)
  end.