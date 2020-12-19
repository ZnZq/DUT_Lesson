-module(mutex_link).
-author("Nikolaenko Oleksiy").
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() -> 
    register(mutex, spawn(?MODULE, init, [])).

stop() -> 
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    receive
        {wait, Pid} -> 
            try
                link(Pid), 
                Pid ! ok, 
                busy(Pid)
            catch
                error:noproc -> free();
                _ -> terminate()
            end;
        stop -> 
            terminate() 
    end.

busy(Pid) -> 
    receive 
        {signal, Pid} -> 
            unlink(Pid), 
            free();
        {'Exit', Pid, _Reason} ->
            free()
    end.

terminate() -> 
    receive 
        {wait, Pid} -> 
            exit(Pid, kill), 
            terminate() 
        after
            0 -> ok 
    end.

