-module(lang).
-author("ZnZ").

-import(lists,[flatten/1]).
-export([calc/1, read/1, print/1]).

token_input() -> [
  {space, "^\\s+", none},
  {float, "^\\-{0,1}\\d+\\.\\d+", none},
  {integer, "^\\-{0,1}\\d+", none},
  {group_start, "^\\(", none},
  {unary_minus, "^\\~\\(", none}
].

token_priority() -> [
  {space, "^\\s+", none},
  {plus, "^\\+", 1},
  {minus, "^\\-", 1},
  {multiply, "^\\*", 2},
  {divide, "^\\/", 2},
  {group_end, "^\\)", 0}
].

print_format() -> [
  {num, fun({_, Value}) -> to_list(Value) end},
  {plus, fun({_, A, B}) -> flatten([print(A), "+", print(B)]) end},
  {minus, fun({_, A, B}) -> flatten([print(A), "-", print(B)]) end},
  {multiply, fun({_, A, B}) -> flatten([print(A), "*", print(B)]) end},
  {divide, fun({_, A, B}) -> flatten([print(A), "/", print(B)]) end},
  {group, fun({_, X}) -> flatten(["(", print(X), ")"]) end},
  {unary_minus, fun({_, X}) -> flatten(["~(", print(X), ")"]) end}
].

to_list(X) when is_integer(X) ->
  integer_to_list(X);
to_list(X) when is_float(X) ->
  float_to_list(X, [{decimals, 4}, compact]);
to_list(_) ->
  {error, unknown_type}.

print(X) ->
  case lists:keysearch(element(1, X), 1, print_format()) of
    {value, {_, Format}} -> Format(X);
    _ -> {error, invalid, X}
  end.

calc(X) when is_list(X) ->
  Read = read(X),
  flatten([print(Read), " = ", to_list(calc(Read))]);
calc(X) when is_tuple(X) ->
  case X of
    {num, I} -> I;
    {plus, A, B} -> calc(A) + calc(B);
    {minus, A, B} -> calc(A) - calc(B);
    {multiply, A, B} -> calc(A) * calc(B);
    {divide, A, B} -> calc(A) / calc(B);
    {unary_minus, A} -> -calc(A);
    {group, A} -> calc(A)
  end.

read(Input) -> read(Input, {[], []}, input).

read([], {[H | _], []}, _) -> H;
read([], {NS, [OSH | OST]}, _) -> read([], {push_ns(NS, OSH), OST}, none);
read(Input, {NS, OS}, input) ->
  case match(Input, token_input()) of
    {space, {_, Tile}} -> read(Tile, {NS, OS}, input);
    {float, {Head, Tile}} -> read(Tile, {[{num, list_to_float(Head)} | NS], OS}, priority);
    {integer, {Head, Tile}} -> read(Tile, {[{num, list_to_integer(Head)} | NS], OS}, priority);
    {group_start, {_, Tile}} -> read(Tile, {NS, [group_start | OS]}, input);
    {unary_minus, {_, Tile}} -> read(Tile, {NS, [unary_minus | OS]}, input);
    _ -> {error, input, {Input}}
  end;
read(Input, {NS, OS}, priority) ->
  case match(Input, token_priority()) of
    {space, {_, Tile}} -> read(Tile, {NS, OS}, priority);
    {group_end, {_, Tile}} ->
      read(Tile, group({NS, OS}), priority);
    {Operator, {_, Tile}} ->
      read(Tile, push_ns_os(Operator, NS, OS), input);
    _ -> {error, priority, {Input}}
  end;
read(I, {NS, OS}, M) -> {error, read, {I, NS, OS, M}}.

group({NS, [HOS | TOS]}) ->
  NNS = push_ns(NS, HOS),
  case NNS of
    [{group, _} | _] -> {NNS, TOS};
    [{unary_minus, _} | _] -> {NNS, TOS};
    _ -> group({push_ns(NS, HOS), TOS})
  end.

push_ns_os(Operator, NS, []) -> {NS, [Operator]};
push_ns_os(Operator, NS, [H | T] = OS) ->
  case compare(Operator, H) of
    big -> {NS, [Operator | OS]};
    low_or_equal -> push_ns_os(Operator, push_ns(NS, H), T)
  end.

compare(O1, O2) ->
  P1 = priority(O1, -1),
  P2 = priority(O2, P1 - 1),
  if
    P1 > P2 -> big;
    true -> low_or_equal
  end.

push_ns([V | Tile], unary_minus) ->
  [{unary_minus, V} | Tile];
push_ns([V | Tile], group_start) ->
  [{group, V} | Tile];
push_ns([V2 | [V1 | Tile]], Operator) ->
  case Operator of
    plus -> [{plus, V1, V2} | Tile];
    minus -> [{minus, V1, V2} | Tile];
    multiply -> [{multiply, V1, V2} | Tile];
    divide -> [{divide, V1, V2} | Tile];
    _ -> {error, push_ns, Operator}
  end.

priority(Operator, Default) ->
  case lists:keysearch(Operator, 1, token_priority()) of
    {value, {_, _, Priority}} -> Priority;
    false -> Default
  end.

match(Input, []) -> {error, nomatch, Input};
match(Input, [{Oper, RegEx, _} | Tile]) ->
  case re:run(Input, RegEx) of
    {match, [{_, Len} | _]} ->
      {Oper, take(Input, Len)};
    nomatch -> match(Input, Tile)
  end.

take(List, Count) -> take(List, Count, []).
take(List, Count, Result)
  when length(Result) =:= Count
  orelse length(List) =:= 0
  -> {Result, List};
take([H | T], Count, Result) ->
  take(T, Count, flatten([Result, [H]])).


%%print_calc(X) ->
%%  flatten([printer(X), " = ", integer_to_list(calc(X))]).
%%
%%calc(X) ->
%%  case X of
%%    {num, I} -> I;
%%    {plus, A, B} -> calc(A) + calc(B);
%%    {minus, A, B} -> calc(A) - calc(B)
%%  end.
%%
%%printer({}) -> "";
%%printer({num, X}) -> integer_to_list(X);
%%printer({plus, _, _} = X) -> g(X);
%%printer({minus, _, _} = X) -> g(X);
%%printer(Other) -> {unknown, Other}.
%%
%%g({num, X}) -> printer(X);
%%g({Op, _, _} = X) ->
%%  case X of
%%    {plus, X1, X2} -> escape(X1) ++ get_op(Op) ++ escape(X2);
%%    {minus, X1, X2} -> escape(X1) ++ get_op(Op) ++ escape(X2)
%%  end.
%%
%%escape({num, _} = X) -> printer(X);
%%escape({plus, _, _} = X) -> parentheses(printer(X));
%%escape({minus, _, _} = X) -> parentheses(printer(X)).
%%
%%parentheses(X) -> flatten(["(", [X], ")"]).
%%
%%get_op(plus) -> " + ";
%%get_op(minus) -> " - ".
%%
%%call() -> read("1 + 2 + 3").
%%
%%token() -> [
%%  {skip, "^\\s+", -1},
%%  {num, "^[0-9]+", 0},
%%  {float, "^[0-9]+\\.[0-9]+", 0},
%%  {plus, "^\\+", 1},
%%  {minus, "^\\-", 1}
%%].
%%
%%read([]) -> "";
%%read(Text) -> read(Text, [], []).
%%
%%read(Text, NumberStack, OperStack) ->
%%  read_oper(get_token(Text), Text, NumberStack, OperStack).
%%
%%read_oper({exit, _Match, Tile}, _Text, NS, OS) when length(OS) > 0 ->
%%  {NNS, NOS} = oper_sum(NS, OS),
%%  read(Tile, NNS, NOS);
%%read_oper({exit, _, _}, _, NS, [])
%%  -> {exit, NS};
%%read_oper({skip, _Match, Tile}, _Text, NS, OS)
%%  -> read(Tile, NS, OS);
%%
%%read_oper({num, _Match, Tile}, _Text, NS, OS)
%%  -> read(Tile, [{num, list_to_integer(_Match)} | NS], OS);
%%read_oper({float, _Match, Tile}, _Text, NS, OS)
%%  -> read(Tile, [{num, list_to_float(_Match)} | NS], OS);
%%
%%read_oper({plus, _Match, Tile}, _Text, NS, []) ->
%%  read(Tile, NS, [plus]);
%%read_oper({plus, _Match, Tile}, _Text, NS, [OSH | _] = OS) ->
%%  {_, {_, _, OSHWeight}} = lists:keysearch(OSH, 1, token()),
%%  {_, {_, _, Weight}} = lists:keysearch(plus, 1, token()),
%%  if
%%    Weight > OSHWeight -> read(Tile, NS, [plus | OS]);
%%    true ->
%%      {NNS, NOS} = oper_sum(NS, OS),
%%      read(Tile, NNS, NOS)
%%  end;
%%
%%read_oper(_, _, _, _) -> what.
%%
%%oper_sum(NS, [plus | TOS]) ->
%%  {[Right, Left], Tile} = take(NS, 2),
%%  {[{plus, Left, Right} | Tile], TOS}.
%%
%%get_token(Text) -> get_token(Text, token()).
%%get_token([], _) -> {exit, [], []};
%%get_token(Text, [{Oper, RegEx, _} | T]) ->
%%  case re:run(Text, RegEx) of
%%    {match, [{_, Len} | _]} ->
%%      {Taked, Tiles} = take(Text, Len),
%%      {Oper, Taked, Tiles};
%%    nomatch -> get_token(Text, T)
%%  end.
%%
%%take(List, Count) -> take(List, Count, []).
%%take(List, Count, Result)
%%  when length(Result) =:= Count
%%  orelse length(List) =:= 0
%%  -> {Result, List};
%%take([H | T], Count, Result) ->
%%  take(T, Count, flatten([Result, [H]])).
%%
%%%%parse(Text) -> parse(Text, {}).
%%%%
%%%%parse([], Result) -> Result;
%%%%parse([C | T], Result) ->
%%%%  if
%%%%    57 >= C andalso C >= 48 -> parse_int([C | T], []);
%%%%    true -> parse(T, Result)
%%%%end.
%%%%
%%%%parse_int([], Result) -> list_to_integer(Result);
%%%%parse_int([C | T], Result) ->
%%%%  if
%%%%    57 >= C andalso C >= 48 -> parse_int(T, Result ++ [C]);
%%%%    true -> {num, parse_int([], Result)}
%%%%end.