-module(shapes).
-author("ZnZ").
-import(math, [sqrt/1]).

-export([area/1]).

area({square, Side})
  -> Side * Side;
area({rectangle, Side1, Side2})
  -> Side1 * Side2;
area({circle, Radius})
  -> math:pi() * Radius * Radius;
area({triangle, A, B, C})
  -> (A + B + C) / 2;
area(_Other)
  -> {error, unknown_shape}.

