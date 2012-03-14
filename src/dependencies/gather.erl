-module(gather).

-export([dependencies/1]).

% Gather module dependencies in App. The source code in App must have been
% compiled with the debug_info flag, or otherwise we can not extract info.
% This will gather only interdependencies between the modules, not
% dependencies on external modules.
dependencies(App) when is_list(App) ->
    BeamDir = filename:join(App, "ebin"),
    case filelib:is_file(BeamDir) of
        true ->
            dps(BeamDir);
        false ->
            dps(App)
    end.

dps(Dir) ->
    Deps = filelib:fold_files(Dir, ".beam", true, fun extract_deps/2, []),
    InternalDeps = [{Name, in_app(Dir, Modules)} || {Name, Modules} <- Deps],
    [D || {_, [_|_]}=D <- InternalDeps]. % Remove empty dependency lists
extract_deps(File, Deps) ->
    Name = list_to_atom(filename:basename(File, ".beam")),
    case (catch extract:abstract_code(File)) of
        {'EXIT', _} -> Deps; % Not compiled with debug_info
        AC -> [{Name, lists:delete(Name, extract:dependencies(AC))}|Deps]
    end.

in_app(BeamDir, Modules) ->
    [M || M <- Modules,
          filelib:is_file(filename:join(BeamDir, atom_to_list(M) ++ ".beam"))].
