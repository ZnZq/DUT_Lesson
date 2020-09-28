-module(db_l).
-author("ZnZ").

-export([new/0, write/3, delete/2, destroy/1, read/2, match/2]).

new() -> [].
destroy(_Db) -> ok.

write(Key, Element, Db)
  -> [{Key, Element} | delete(Key, Db)].

read(Key, Db) ->
  case lists:keysearch(Key, 1, Db) of
    {_, {_, Value}} -> {ok, Value};
    _ -> {error, instance}
  end.

match(_, []) -> [];
match(Element, [H | T])
  -> case lists:keysearch(Element, 2, [H]) of
       {_, {Key, _}} -> [Key | match(Element, T)];
       _ -> match(Element, T)
     end.

delete(Key, Db)
  -> lists:keydelete(Key, 1, Db).
