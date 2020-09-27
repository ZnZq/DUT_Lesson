-module(arr_r).
-author("ZnZ").

-import(list, [create/1]).
-import(db_r, [filter/3]).
-export([all/1, odd/1, even/1, len/1, append/2]).

all(N) ->
  {ok, len(db_r:filter(
    list:create(N),
    fun(_) -> true end,
    fun(X) -> print(X) end
  ))}.

odd(N) ->
  {ok, len(db_r:filter(
    list:create(N),
    fun(X) -> X rem 2 =/= 0 end,
    fun(X) -> print(X) end
  ))}.

even(N) ->
  {ok, len(db_r:filter(
    list:create(N),
    fun(X) -> X rem 2 =:= 0 end,
    fun(X) -> print(X) end
  ))}.

print(X) -> io:format("Number: ~p~n", [X]).

len(L) -> len(L, 0).
len([], Total) -> Total;
len([_ | T], Total) -> len(T, Total + 1).

append([], Tail) -> Tail;
append([H | T], Tail)
  -> [H | append(T, Tail)].