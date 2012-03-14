-module(visualize).

-export([digraph/1]).

digraph(Nodes) ->
    "digraph G { \n" ++
    dg(Nodes) ++
    "}".

dg([]) -> "";
dg([{Node, Edges}|T]) ->
    dg(to_list(Node), Edges) ++
    dg(T).

dg(_, []) -> "";
dg(Node, [E|T]) ->
    Node ++ " -> " ++ to_list(E) ++ ";\n" ++ dg(Node, T).

to_list({Edge, Label}) ->
    to_list(Edge) ++ " [label=" ++ to_list(Label) ++ "]";
to_list(L) when is_list(L) -> "\"" ++ L ++ "\"";
to_list(A) when is_atom(A) -> to_list(atom_to_list(A));
to_list(Pid) when is_pid(Pid) -> to_list(pid_to_list(Pid)).
