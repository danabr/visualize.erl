-module(crossring).
-include("include/analyze.hrl").
-export([start/2]).

start(NProcs, NMsgs) when NMsgs > 0, NProcs > 2 ->
  ProcsToCreate = NProcs-1,
  ProcsInFirstRing = ProcsToCreate div 2,
  ProcsInSecondRing = ProcsToCreate - ProcsInFirstRing, 
  io:format("~p procs in first half~n", [ProcsInFirstRing]),
  io:format("~p procs in second half~n", [ProcsInSecondRing]),
  MasterProc = spawn(fun master_proc/0),
  [LastInFirstRing|_] = make_procs(ProcsInFirstRing, MasterProc),
  [LastInSecondRing|_] = make_procs(ProcsInSecondRing, MasterProc),
  MasterProc ! {start, LastInFirstRing, LastInSecondRing},
  send_msgs(NMsgs, MasterProc),
  MasterProc ! stop,
  ok. 

master_proc() ->
  receive
    {start, First, Second} ->
      master_proc_send(First, Second)
  end.

master_proc_send(First, Second) ->
  receive
    {msg, Msg} ->
      First ! {msg, Msg},
      master_proc_await(First, Second, Msg);
    stop ->
      io:format("Master proc terminates~n"),
      First ! stop,
      Second ! stop
  end.

master_proc_await(First, Second, Msg) ->
  receive
    {msg, Msg} ->
      io:format("Msg ~p has passed half way through~n", [Msg]),
      Second ! {msg, Msg},
      master_proc_wait_last(First, Second, Msg)
  end.

master_proc_wait_last(First, Second, Msg) ->
    receive
      {msg, Msg} ->
        master_proc_send(First, Second)
    end.

send_msgs(0, _Proc) ->
  ok;
send_msgs(N, Proc) -> 
  Proc ! {msg, N},
  send_msgs(N-1, Proc).

make_procs(N, Next) ->
  make_procs(N, Next, []).

make_procs(0, _Next, Procs) ->
  Procs;
make_procs(N, Next, Procs) ->
  Pid = spawn(fun() -> loop(Next) end),
  make_procs(N-1, Pid, [Pid|Procs]).

loop(Next) ->
  receive
    {msg, Msg} ->
      io:format("~p received ~p~n", [self(), Msg]),
      Next ! {msg, Msg},
      loop(Next);
    stop ->
      io:format("~p stops~n", [self()]),
      Next ! stop
  end.

