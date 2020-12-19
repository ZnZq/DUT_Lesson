-module(database).
-author("Nikolaenko Oleksiy").
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).


start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args) ->
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.

write(Key, Element) ->
    gen_server:call(?MODULE, {write, Key, Element}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
    gen_server:call(?MODULE, {read, Key}).

match(Element) ->
    gen_server:call(?MODULE, {match, Element}).

handle_call({write, Key, Element}, _From, LoopData) ->
    {Reply, NewState} = database_gen:write({Key, Element}, LoopData),
    {reply, Reply, NewState};

handle_call({read, Key}, _From, LoopData) ->
    {Reply, NewState} = database_gen:read(Key, LoopData), 
    {reply, Reply, NewState};

handle_call({match, Element}, _From, LoopData) ->
    {Reply, NewState} = database_gen:match(Element, LoopData),
    {reply, Reply, NewState};

handle_call({delete, Key}, _From, LoopData) ->
    {Reply, NewState} = database_gen:delete(Key, LoopData),
    {reply, Reply, NewState}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.
