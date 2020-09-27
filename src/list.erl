-module(list).
-author("ZnZ").

-export([create/1, reverse_create/1, filter/2, reverse/1]).

create(N) -> create(N, []).
reverse_create(N) -> reverse(create(N)).
filter(L, N) -> [X || X <- L, X =< N], list_filter(L, fun(X) -> X =< N end).
reverse(L) -> reverse(L, []).

create(0, Total) -> Total;
create(N, Total) -> create(N - 1, [N | Total]).

reverse([], R) -> R;
reverse(L, R) -> [H | T] = L, reverse(T, [H | R]).

list_filter(List, Filter) -> list_filter(List, Filter, []).
list_filter([], _, Result) -> Result;
list_filter(List, Filter, Result) ->
  [H | T] = List,
  case Filter(H) of
    true -> [H | list_filter(T, Filter)];
    _ -> list_filter(T, Filter, Result)
  end.