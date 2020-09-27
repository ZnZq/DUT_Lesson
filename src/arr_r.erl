-module(arr_r).
-author("ZnZ").

-import(list, [create/1]).
-import(db_r, [filter/3]).
-export([all/1, odd/1, even/1, len/1, append/2]).

all(N) ->
  { ok, len(num_list(N, fun(_) -> true end)) }.

odd(N) ->
  { ok, len(num_list(N, fun(X) -> X rem 2 =/= 0 end)) }.

even(N) ->
  { ok, len(num_list(N, fun(X) -> X rem 2 =:= 0 end)) }.

num_list(N, Filter) ->
  db_r:filter(
    list:create(N),
    Filter,
    fun(X) -> print(X) end
  ).

print(X) -> io:format("Number: ~p~n", [X]).

len(L) -> len(L, 0).
len([], Total) -> Total;
len([_ | T], Total) -> len(T, Total + 1).

append([], Tail) -> Tail;
append([H | T], Tail)
  -> [H | append(T, Tail)].