-module(my_supervisor).
-author("Nikolaenko Oleksiy").
-export([start_link/2, stop/1]).
-export([init/1]).
-export([start_child/4, stop_child/2]).

-define(MaxRestart, 5).
-define(MaxTime, 300).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{Id, Mod, Fun, Args, Type} | ChildSpecList]) ->
    case (catch apply(Mod, Fun, Args)) of
        {ok, Pid} -> 
            [{Pid, {Id, Mod, Fun, Args, Type, []}}|start_children(ChildSpecList)];
        _ -> 
            start_children(ChildSpecList) 
    end.

restart_child(Pid, ChildList, Reason) ->
    {value, {Pid, {Id, Mod, Fun, Args, Type, RestartTime}}} = lists:keysearch(Pid, 1, ChildList),
    case {Type, Reason} of 
        {transient, normal} -> 
            true;
        _Other -> 
            NewRestartTime = restart_update(RestartTime),
            if 
                length(NewRestartTime) > ?MaxRestart ->
                    io:format("error: restart frequency exceeded (Pid=~p). ~n", [Pid]),
                    lists:keydelete(Pid, 1, ChildList);
                true ->
                    {ok, NewPid} = apply(Mod, Fun, Args), 
                    [{NewPid, {Id, Mod, Fun, Args, Type, NewRestartTime}}|lists:keydelete(Pid, 1, ChildList)]
            end
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, Reason} -> 
            NewChildList = restart_child(Pid, ChildList, Reason), 
            loop(NewChildList);
        {start_child, From, Mod, Fun, Args} ->
            NewChildList = case (catch apply(Mod, Fun, Args)) of
                {ok, Pid} -> 
                    Id = make_ref(),
                    From ! {reply, {Id, Pid}},
                    [{Pid, {Id, Mod, Fun, Args, transient, []}}|ChildList];
                Other ->
                    From ! {reply, {error, Other}},
                    ChildList
            end,
            loop(NewChildList);
        {stop_child, From, Id} ->
            DelList = lists:filter(fun({_, {DId, _, _, _, _, _}}) -> DId == Id end, ChildList),
            NewChildList = case DelList of 
                [{Pid, _}|_] ->
                    From ! {reply, ok},
                    exit(Pid, kill),
                    lists:keydelete(Pid, 1, ChildList);
                _ ->
                    From ! {reply, {error, not_found}},
                    ChildList
            end,
            loop(NewChildList);
        {stop, From} -> 
            From ! {reply, terminate(ChildList)}
    end.


stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

restart_update(RestartTime) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    Now = MegaSecs * 1000 * 1000 + Secs,
    [Now | lists:filter(fun(E) -> (Now-E) < ?MaxTime end, RestartTime)].

start_child(Name, Mod, Fun, Args) ->
    Name ! {start_child, self(), Mod, Fun, Args},
    receive
        {reply, Reply} ->
            Reply
    end.

stop_child(Name, Id) ->
    Name ! {stop_child, self(), Id},
    receive
        {reply, Reply} ->
            Reply
    end.
