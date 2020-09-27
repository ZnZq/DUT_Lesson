-module(db_r).
-author("ZnZ").

-export([new/0, write/3, delete/2, destroy/1, read/2, match/2]).

new() -> [].
destroy(_Db) -> ok.

write(Key, Element, Db) ->
  Array = delete(Key, Db),
  [{Key, Element} | Array].

read(Key, Db) ->
  E = filter(Db, fun(X) -> {K, _} = X, K =:= Key end, fun(X) -> X end),
  case E of
    [] -> {error, instance};
    _ -> [H | _] = E, {ok, element(2, H)}
  end.

match(Element, Db) -> filter(
    Db,
    fun(X) -> {_, V} = X, V =:= Element end,
    fun(X) -> {K, _} = X, K end
  ).

delete(Key, Db)
  -> filter(Db, fun(X) -> {K, _} = X, K =/= Key end, fun(X) -> X end).

filter(List, Filter, Expr) -> filter(List, Filter, Expr, []).
filter([], _, _, Result) -> Result;
filter(List, Filter, Expr, Result) ->
  [H | T] = List,
  case Filter(H) of
    true -> [Expr(H) | filter(T, Filter, Expr)];
    _ -> filter(T, Filter, Expr, Result)
  end.
