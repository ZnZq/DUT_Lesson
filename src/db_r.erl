-module(db_r).
-author("ZnZ").

-export([new/0, write/3, delete/2, destroy/1, read/2, match/2, filter/2, filter/3]).

new() -> [].
destroy(_Db) -> ok.

write(Key, Element, Db) ->
  Array = delete(Key, Db),
  [{Key, Element} | Array].

read(Key, Db) ->
  E = filter(Db, fun({K, _}) -> K =:= Key end),
  case E of
    [] -> {error, instance};
    [{_, Value} | _] -> {ok, Value}
  end.

match(Element, Db) -> filter(
    Db,
    fun({_, V}) -> V =:= Element end,
    fun({K, _}) -> K end
  ).

delete(Key, Db)
  -> filter(Db, fun({K, _}) -> K =/= Key end).

filter(List, Filter) -> filter(List, Filter, fun(X) -> X end).

filter([], _, _) -> [];
filter(List, Filter, Expr) ->
  [H | T] = List,
  case Filter(H) of
    true -> [Expr(H) | filter(T, Filter, Expr)];
    _ -> filter(T, Filter, Expr)
  end.
