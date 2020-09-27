-module(list).
-author("ZnZ").

-import(db_r, [filter/3]).
-import(arr_r, [append/2]).
-export([create/1, reverse_create/1, filter/2, reverse/1, concatenate/1, flatten/1]).

create(N) -> create(N, []).
reverse_create(N) -> reverse(create(N)).
filter(L, N) -> db_r:filter(L,
  fun(X) -> X =< N end,
  fun(X) -> X end
).
reverse(L) -> reverse(L, []).
concatenate(L) -> concatenate(L, []).
flatten(L) -> flatten(L, []).

create(0, Total) -> Total;
create(N, Total) -> create(N - 1, [N | Total]).

reverse([], R) -> R;
reverse([H | T], R) -> reverse(T, [H | R]).

concatenate([], Result) -> Result;
concatenate([H | T], Result)
  -> concatenate(T, Result ++ H).

flatten([], Result) -> Result;
flatten([H | T], Result) ->
  if
    is_list(H) -> flatten(T, flatten(H, Result));
    true -> flatten(T, arr_r:append(Result, [H]))
  end.