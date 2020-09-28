-module(lang).
-author("ZnZ").

-import(lists,[flatten/1]).
-export([printer/1, calc/1, print_calc/1]).

print_calc(X) ->
  flatten([printer(X), " = ", integer_to_list(calc(X))]).

calc(X) ->
  case X of
    {num, I} -> I;
    {plus, A, B} -> calc(A) + calc(B);
    {minus, A, B} -> calc(A) - calc(B)
  end.

printer({}) -> "";
printer({num, X}) -> integer_to_list(X);
printer({plus, _, _} = X) -> g(X);
printer({minus, _, _} = X) -> g(X);
printer(Other) -> {unknown, Other}.

g({num, X}) -> printer(X);
g({Op, _, _} = X) ->
  case X of
    {plus, X1, X2} -> escape(X1) ++ get_op(Op) ++ escape(X2);
    {minus, X1, X2} -> escape(X1) ++ get_op(Op) ++ escape(X2)
  end.

escape({num, _} = X) -> printer(X);
escape({plus, _, _} = X) -> parentheses(printer(X));
escape({minus, _, _} = X) -> parentheses(printer(X)).

parentheses(X) -> flatten(["(", [X], ")"]).

get_op(plus) -> " + ";
get_op(minus) -> " - ".



%%parse(Text) -> parse(Text, {}).
%%
%%parse([], Result) -> Result;
%%parse([C | T], Result) ->
%%  if
%%    57 >= C andalso C >= 48 -> parse_int([C | T], []);
%%    true -> parse(T, Result)
%%end.
%%
%%parse_int([], Result) -> list_to_integer(Result);
%%parse_int([C | T], Result) ->
%%  if
%%    57 >= C andalso C >= 48 -> parse_int(T, Result ++ [C]);
%%    true -> {num, parse_int([], Result)}
%%end.