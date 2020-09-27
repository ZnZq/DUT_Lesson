-module(sum).
-author("ZnZ").

-export([sum/1]).

sum(N) -> summer(N, 0).

summer(0, Total) -> Total;
summer(N, Total) -> summer(N - 1, Total + N).