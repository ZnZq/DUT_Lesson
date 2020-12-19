-module(index2).
-author("Nikolaenko Oleksiy").
-export([processWords/2]).

processWords(Words, N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          case ets:lookup(indexTable, Normalise) of 
            [] -> 
              ets:insert(indexTable, {Normalise, [N]});
            [{_, Ns}] ->
                lists:insert(indexTable, {Normalise, [N|Ns]})
          end;
        true -> ok
      end,
      processWords(Rest, N)
  end.



