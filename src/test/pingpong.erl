-module(pingpong).

-export([start/0]).

-include("include/analyze.hrl").

start() ->
  spawn(fun run/0).

run() ->
  Self = self(),
  register(ping, Self),
  Fun = fun() ->
            register(pong, self()),
            Self ! lists:reverse([1,2,3])
        end,
  erlang:spawn(Fun),
  receive
      [3,2,1] -> io:format("Done!")
  end.
