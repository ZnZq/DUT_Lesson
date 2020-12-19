-module(ind).
-author("ZnZ").

-export([read/1]).

read(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try index(Device, [])
  after file:close(Device)
  end.

index(Device, Db) ->
  Line = case io:get_line(Device, "") of
    eof  -> [];
    L -> L
  end,
  case re:run(Line, "\\w+", [global]) of
    {match, List} -> q;
    nomatch -> q
  end.