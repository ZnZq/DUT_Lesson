-module(arr).
-author("ZnZ").

-import(list, [create/1]).
-export([all/1, odd/1, even/1]).

all(N) ->
  Array = [print(X) || X <- list:create(N)],
  {ok, length(Array)}.

odd(N) ->
  Array = [print(X) || X <- list:create(N), X rem 2 =/= 0],
  {ok, length(Array)}.

even(N) ->
  Array = [print(X) || X <- list:create(N), X rem 2 =:= 0],
  {ok, length(Array)}.

print(X) -> io:format("Number: ~p~n", [X]).