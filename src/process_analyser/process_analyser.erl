-module(process_analyser).

% External API
-export([start/0, stop/0, analyse/0, flow/0]).

% Analysis API
-export([spawn/1, spawn_link/1, send/2, register/2]).

-compile({no_auto_import,[spawn/1, spawn_link/1, register/2]}).

%%% External API
start() ->
    Fun = fun() ->
        erlang:register(process_analyser, self()),
        loop(ets:new(process_analyser, [bag]))
    end,
    erlang:spawn(Fun).

stop() ->
    process_analyser ! stop.

analyse() ->
    Ref = make_ref(),
    process_analyser ! {get_data, Ref, self()},
    receive
        {Ref, Reply} -> analyse(Reply)
    after 1000 ->
      {error, no_response}
    end.

analyse(Data) ->
    Pid2Name = dict:from_list([{Pid, Name} || {Pid, {name, Name}} <- Data]),
    analyse(Data, Pid2Name).

% Translate Pids to registered names, and filter out name properties
analyse([], _) -> [];
analyse([{_, {name, _}}|T], Pid2Name) ->
    analyse(T, Pid2Name);
analyse([{N1, {PropName, N2}}|T], Pid2Name) ->
    New = {translate(N1, Pid2Name), {translate(N2, Pid2Name), PropName}},
    [New|analyse(T, Pid2Name)];
analyse([{N1, N2}|T], Pid2Name) ->
    New = {translate(N1, Pid2Name), translate(N2, Pid2Name)},
    [New|analyse(T, Pid2Name)].

translate(Pid, Pid2Name) when is_pid(Pid) ->
    case dict:find(Pid, Pid2Name) of
        {ok, Name} ->
            atom_to_list(Name) ++ " (" ++ pid_to_list(Pid) ++ ")";
        error ->
            Pid
    end;
translate(Name, _) -> Name.

flow() ->
    Data = analyse(),
    ToDict = fun({From, Edge}, Acc) -> dict:append(From, Edge, Acc) end,
    Dict = lists:foldl(ToDict, dict:new(), Data),
    dict:to_list(Dict).

%%% Analysis API
spawn(Args) ->
    Pid = apply(erlang, spawn, Args),
    process_analyser ! {register_spawn, self(), Pid},
    Pid.

spawn_link(Args) ->
    Pid = apply(erlang, spawn_link, Args),
    process_analyser ! {register_spawn, self(), Pid},
    Pid.

send(To, Msg) ->
    To ! Msg,
    process_analyser ! {register_send, self(), To},
    Msg.

register(Name, Pid) when is_pid(Pid) ->
    process_analyser ! {register_register, Pid, Name},
    erlang:register(Name, Pid);
register(Name, Any) ->
    erlang:register(Name, Any).

%%% Internal functions
loop(ID) ->
    receive
        {register_spawn, ParentPid, ProcPid} ->
            io:format("Spawning ~p from ~p!~n", [ProcPid, ParentPid]),
            ets:insert(ID, {ParentPid, {parent, ProcPid}}),
            loop(ID);
        {register_send, From, To} ->
            io:format("Sending message from ~p to ~p~n", [From, To]),
            ets:insert(ID, {From, To}),
            loop(ID);
        {register_register, Pid, Name} ->
            io:format("~p now registered as ~p~n", [Pid, Name]),
            ets:insert(ID, {Pid, {name, Name}}),
            loop(ID);
        {get_data, Ref, Pid} ->
            Pid ! {Ref, ets:tab2list(ID)}, 
            loop(ID);
        stop ->
            ok
    end.
