%% Implements a parse transform for analysing communication between
%% processes. It transforms the following functions:
%%   x The ! (bang) operator
%%   x spawn
%%   x spawn_link
%%   x register
%%   * gen_server:call
%%   * gen_server:cast
%%   * gen_server:reply
%%   * gen_server:start
%%   * gen_server:start_link

-module(process_communication_analysis_transform).

-export([parse_transform/2]).

-record(state, {module, func}).

%%%
%%% Exported functions
%%%

-spec(parse_transform(Forms, Options) -> Forms2 when
      Forms :: [erl_parse:abstract_form()],
      Forms2 :: [erl_parse:abstract_form()],
      Options :: [Option],
      Option :: type_checker | compile:option()).

parse_transform(Forms, _Options) ->
    io:format("Performing parse transform!~n"),
    forms(Forms, #state{}).

forms({op, _, '!', Lhs, Rhs}, State) ->
    NewLhs = forms(Lhs, State),
    NewRhs = forms(Rhs, State),
    analysis_for(send, [NewLhs, NewRhs], State);
%% Calls
% spawn
forms({call, _, {atom, _, spawn}, Args}, State) ->
    analysis_for_generic(spawn, Args, State);
forms({call, _, {remote,_,{atom,_,erlang}, {atom,_,spawn}}, Args},S)->
    analysis_for_generic(spawn, Args, S);
% spawn_link
forms({call, _, {atom, _, spawn_link}, Args}, State) ->
    analysis_for_generic(spawn_link, Args, State);
forms({call, _, {remote,_,{atom,_,erlang},{atom,_,spawn_link}}, Args},S)->
    analysis_for_generic(spawn_link, Args, S);
% register
forms({call, _, {atom, _, register}, Args}, State) ->
    analysis_for(register, Args, State);
forms({call, _, {remote,_,{atom,_,erlang},{atom,_,register}}, Args},S)->
    analysis_for(register, Args, S);

% Catch all
forms(List, State) when is_list(List) ->
    [forms(F, State) || F <- List];
forms(Tuple, State) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Transformed = forms(List, State),
    list_to_tuple(Transformed);
forms(Any, _State) ->
    % io:format("Catchall:~n~p~n", [Any]),
    Any.
% forms([{attribute, _Line, module, Mod}=F|T], State) ->
%     [F|forms(T, State#state{module=Mod})];
% forms([{function, Line, Name, Arity, Clauses}|T], State) ->
%     NameStr = atom_to_list(Name) ++ "/" ++ integer_to_list(Arity),
%     State2 = State#state{func=NameStr},
%     NewF = {function, Line, Name, Arity, forms(Clauses, State2)},
%     [NewF|forms(T, State)];
% forms([{'fun', Line, {clauses, Clauses}}|T], State) ->
%     NewClauses = forms(Clauses, State),
%     NewF = {'fun', Line, {clauses, NewClauses}},
%     [NewF|forms(T, State)];
% forms([{clause, Line, Vars, Guards, Exprs}|T], State) ->
%     NewF = {clause, Line, Vars, Guards, forms(Exprs, State)},
%     [NewF|forms(T, State)];
% forms([{match, Line, Lhs, Rhs}|T], State) ->
%     NewLhs = hd(forms([Lhs], State)),
%     NewRhs = hd(forms([Rhs], State)),
%     NewF = {match, Line, NewLhs, NewRhs},
%     [NewF|forms(T, State)];
% % Noise
% forms([{attribute, _Line, _, _}=F|T], State) ->
%     [F|forms(T, State)];
% forms([{var, _Line, _Name}=F|T], State) ->
%     [F|forms(T, State)];
% forms([{eof, _Line}], _State) -> [];
% % Catch all
% forms([F|T], State) ->
%     io:format("Ignoring (~p):~n~p~n", [State, F]),
%     [F|forms(T, State)].

%%% Helpers
analysis_for(FunName, Args, State) ->
    {call, 0,
        {remote, 0, {atom, 0, process_analyser}, {atom, 0, FunName}},
        forms(Args, State)}.
analysis_for_generic(FunName, Args, State) ->
    {call, 0,
        {remote, 0, {atom, 0, process_analyser}, {atom, 0, FunName}},
        [to_cons(forms(Args, State))]}.

to_cons([]) -> {nil, 0};
to_cons([H|T]) -> {cons, 0, H, to_cons(T)}. 

