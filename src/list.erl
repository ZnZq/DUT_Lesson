-module(list).
-author("ZnZ").

-export([create/1, reverse_create/1]).

create(N) -> create_list(N, []).
reverse_create(N) -> create_reverse_list(N, []).

create_list(0, Total) -> Total;
create_list(N, Total)
  -> create_list(N - 1, [N | Total]).

create_reverse_list(0, Total) -> Total;
create_reverse_list(N, Total)
  -> create_reverse_list(N - 1, Total ++ [N]).