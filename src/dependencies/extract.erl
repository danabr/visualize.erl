-module(extract).

-export([abstract_code/1, functions/1, calls/1, dependencies/1]).

abstract_code(Mod) ->
   {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod,
                                                          [abstract_code]),
   AC.

functions(AC) ->
    functions(AC, []).

functions([], Acc) -> Acc;
functions([{function, _, _, _, _}=F|T], Acc) ->
    functions(T, [F|Acc]);
functions([_|T], Acc) ->
    functions(T, Acc).

dependencies(AC) ->
    extract_modules(calls(AC)).

extract_modules(Calls) ->
    extract_modules(Calls, []).

extract_modules([], Acc) -> lists:usort(Acc);
extract_modules([{remote, _Line, ModExpr, _FunExpr}|T], Acc) ->
    extract_modules(T, [extract_module(ModExpr)|Acc]);
extract_modules([_|T], Acc) ->
    extract_modules(T, Acc).

extract_module({atom, _Line, Name}) -> Name;
extract_module(_C) -> 'UNKNOWN'.

calls(AC) ->
    Fs = functions(AC),
    calls(Fs, []).

calls([], Acc) -> Acc;
calls([{function, _Line, _Name, _Arity, Clauses}|T], Acc) ->
    calls(T, calls(Clauses, Acc));
calls([{clause, _Line, _Vars, _Guards, Exprs}|T], Acc) ->
    calls(T, calls(Exprs, Acc));
calls([{call, _Line, {'fun', _Line, _}=F, Args}|T], Acc) ->
    calls(T, calls([F|Args], Acc));
calls([{call, _Line, Fun, Args}|T], Acc) ->
    calls(T, [Fun|calls(Args, Acc)]);
calls([{op, _Line, _Op, Arg}|T], Acc) ->
    calls(T, calls([Arg], Acc));
calls([{op, _Line, _Op, Arg1, Arg2}|T], Acc) ->
    calls(T, calls([Arg1, Arg2], Acc));
calls([{'receive', _Line, Clauses}|T], Acc) ->
    calls(T, calls(Clauses, Acc));
calls([{'receive', _Line, RecClauses, _Tmo, TmoClauses}|T], Acc) ->
    calls(T, calls(TmoClauses ++ RecClauses, Acc));
calls([{'case', _Line, Subject, Clauses}|T], Acc) ->
    calls(T, calls([Subject|Clauses], Acc));
calls([{'if', _Line, Clauses}|T], Acc) ->
    calls(T, calls(Clauses, Acc));
calls([{'catch', _Line, Risky}|T], Acc) ->
    calls(T, calls([Risky], Acc));
calls([{'try', _Line, Expr, CaseClauses, CatchClauses, _What}|T], Acc) ->
    calls(T, calls(Expr ++ CaseClauses ++ CatchClauses, Acc));
calls([{cons, _Line, Head, Tail}|T], Acc) ->
    calls(T, calls([Head, Tail], Acc));
calls([{match, _Line, Left, Right}|T], Acc) ->
    calls(T, calls([Left, Right], Acc));
calls([{'fun', _Line, {clauses, Clauses}}|T], Acc) ->
    calls(T, calls(Clauses, Acc));
calls([{lc, _Line, Res, Generators}|T], Acc) ->
    calls(T, calls([Res|Generators], Acc));
calls([{generate, _Line, Element, Origin}|T], Acc) ->
    calls(T, calls([Element,Origin], Acc));
calls([{block, _Line, Matches}|T], Acc) ->
    calls(T, calls(Matches, Acc));
calls([{tuple, _Line, Contents}|T], Acc) ->
    calls(T, calls(Contents, Acc));
% Noise
calls([{record, _Line, _Name, _Fields}|T], Acc) ->
    calls(T, Acc);
calls([{record, _Line, _Var, _Name, _Fields}|T], Acc) ->
    calls(T, Acc);
calls([{record_field, _Line, _Var, _Rec, _Field}|T], Acc) ->
    calls(T, Acc);
calls([{record_index, _Line, _Rec, _Field}|T], Acc) ->
    calls(T, Acc);
calls([{atom, _Line, _Name}|T], Acc) ->
    calls(T, Acc);
calls([{var, _Line, _Var}|T], Acc) ->
    calls(T, Acc);
calls([{integer, _Line, _Value}|T], Acc) ->
    calls(T, Acc);
calls([{float, _Line, _Value}|T], Acc) ->
    calls(T, Acc);
calls([{string, _Line, _Value}|T], Acc) ->
    calls(T, Acc);
calls([{char, _Line, _Value}|T], Acc) ->
    calls(T, Acc);
calls([{nil, _Line}|T], Acc) ->
    calls(T, Acc);
calls([{'fun', _Line, _FunInfo}|T], Acc) ->
    calls(T, Acc);
% Catch all cases we do not handle yet
calls([H|T], Acc) ->
    io:format("Skipping:~n~p~n", [H]),
    calls(T, Acc).
