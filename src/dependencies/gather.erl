-module(gather).

-export([dependencies/1, dependencies/2]).

% Same as dependencies(App, internal).
% This will gather only interdependencies between the modules, not
% dependencies on external modules.
dependencies(App) when is_list(App) ->
    dependencies(App, internal).

% Gather module dependencies in App. The source code in App must have been
% compiled with the debug_info flag, or otherwise we can not extract info.
% Filter = full | not_loaded | internal
dependencies(App, Filter) when is_list(App) andalso
                                (Filter =:= internal orelse
                                 Filter =:= not_loaded orelse 
                                 Filter =:= full) ->
    BeamDir = filename:join(App, "ebin"),
    case filelib:is_file(BeamDir) of
        true ->
            dps(BeamDir, Filter);
        false ->
            dps(App, Filter)
    end.

dps(Dir, Filter) ->
    Deps = filelib:fold_files(Dir, ".beam", true, fun extract_deps/2, []),
    FilteredDeps = filter(Filter, Deps, Dir),
    [D || {_, [_|_]}=D <- FilteredDeps]. % Remove empty dependency lists

extract_deps(File, Deps) ->
    Name = list_to_atom(filename:basename(File, ".beam")),
    case (catch extract:abstract_code(File)) of
        {'EXIT', _} -> Deps; % Not compiled with debug_info
        AC -> [{Name, lists:delete(Name, extract:dependencies(AC))}|Deps]
    end.

filter(internal, Deps, Dir) ->
    [{Name, in_app(Dir, Modules)} || {Name, Modules} <- Deps];
filter(not_loaded, Deps, _Dir) ->
    [{Name, not_loaded(Modules)} || {Name, Modules} <- Deps];
filter(full, Deps, _Dir) ->
    Deps.

in_app(BeamDir, Modules) ->
    [M || M <- Modules,
          filelib:is_file(filename:join(BeamDir, atom_to_list(M) ++ ".beam"))].

not_loaded(Modules) ->
    [M || M <- Modules, code:is_loaded(M) =:= false].
