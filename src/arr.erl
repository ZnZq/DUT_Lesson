-module(arr).
-author("ZnZ").

-import(list, [create/1]).
-import(arr_r, [len/1]).
-export([all/1, odd/1, even/1]).

all(N) ->
  Array = [print(X) || X <- list:create(N)],
  {ok, arr_r:len(Array)}.

odd(N) ->
  Array = [print(X) || X <- list:create(N), X rem 2 =/= 0],
  {ok, arr_r:len(Array)}.

even(N) ->
  Array = [print(X) || X <- list:create(N), X rem 2 =:= 0],
  {ok, arr_r:len(Array)}.

print(X) -> io:format("Number: ~p~n", [X]).