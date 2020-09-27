-module(db).
-author("ZnZ").

-export([new/0, write/3, delete/2, destroy/1, read/2, match/2]).

new() -> [].
destroy(_) -> ok.

write(Key, Element, Db) ->
  Array = delete(Key, Db),
  [{Key, Element}] ++ Array.

read(Key, Db) ->
  E = [X || X <- Db, element(1, X) =:= Key],
  case E of
    [] -> {error, instance};
    _ -> [H | _] = E, {ok, element(2, H)}
  end.

match(Element, Db)
  -> [element(1, X) || X <- Db, element(2, X) =:= Element].

delete(Key, Db)
  -> [X || X <- Db, element(1, X) =/= Key].