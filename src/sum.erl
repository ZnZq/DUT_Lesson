-module(sum).
-author("ZnZ").

-export([sum/1, sum/2]).

sum(N) -> summer(1, N, 0).
sum(X, N) -> summer(X, N, 0).

summer(X, X, Total) -> Total + X;
summer(X, N, Total) -> summer(X, N - 1, Total + N).