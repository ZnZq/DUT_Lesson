-module(db_p).
-author("ZnZ").

-export([new/0, write/3, delete/2, destroy/1, read/2, match/2, loop/1]).

new() -> spawn(db_p, loop, [[]]).
destroy(Db) -> Db ! destroy, ok.

write(Key, Element, Db)
  -> send(Db, {write, Key, Element}).

read(Key, Db)
  -> send(Db, {read, Key}).

match(Element, Db)
  -> send(Db, {match, Element}).

delete(Key, Db)
  -> send(Db, {delete, Key}).

send(Pid, Message) ->
  IsAlive = is_process_alive(Pid),
  if
    not IsAlive -> {error, db_not_found};
    true -> Self = self(),
      Pid ! list_to_tuple([Self | tuple_to_list(Message)]),
      receive
        {Self, Result} -> Result
      end
  end.

loop(Db) ->
  receive
    destroy -> true;
    {Pid, delete, Key} ->
      Delete = delete_p(Key, Db),
      Pid ! {Pid, Delete},
      loop(Delete);
    {Pid, write, Key, Element} ->
      Write = write_p(Key, Element, Db),
      Pid ! {Pid, Write},
      loop(Write);
    {Pid, read, Key} ->
      Read = read_p(Key, Db),
      Pid ! {Pid, Read},
      loop(Db);
    {Pid, match, Element} ->
      Match = match_p(Element, Db),
      Pid ! {Pid, Match},
      loop(Db);
    _Other -> unknown
  end.

delete_p(Key, Db)
  -> [X || X <- Db, element(1, X) =/= Key].

write_p(Key, Element, Db) ->
  Array = delete_p(Key, Db),
  [{Key, Element} | Array].

read_p(Key, Db) ->
  case [X || X <- Db, element(1, X) =:= Key] of
    [] -> {error, instance};
    [{_, Value} | _] -> {ok, Value}
  end.

match_p(Element, Db)
  -> [element(1, X) || X <- Db, element(2, X) =:= Element].