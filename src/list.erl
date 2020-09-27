-module(list).
-author("ZnZ").

-export([create/1, reverse_create/1, filter/2, reverse/1]).

create(N) -> create(N, []).
reverse_create(N) -> reverse(create(N)).
filter(L, N) -> [X || X <- L, X =< N].
reverse(L) -> reverse(L, []).

create(0, Total) -> Total;
create(N, Total) -> create(N - 1, [N | Total]).

reverse([], R) -> R;
reverse(L, R) -> [H | T] = L, reverse(T, [H] ++ R).