-module(database_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  UsrChild = {database, {database, start, []},
              permanent, 2000, worker, [database, database_sup]},
  {ok, {{one_for_all, 1, 3}, [UsrChild]}}.
