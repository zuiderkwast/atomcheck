-module(atomcheck).

-export([main/1, collect/1, once/1, twice/1]).
-mode(compile).

%% Escript entry
main(Args) ->
    %logger:info("Collecting data"),
    Counters = collect(Args),
    % io:format("~p~n", [Counters]),
    io:format("Atoms used once: ~p~n", [once(Counters)]),
    io:format("Atoms used twice: ~p~n", [twice(Counters)]),
    ok.

collect(Beams) -> collect_from_beams(Beams, new_counter_set()).
once(Counters) -> get_keys_with_exact_count(1, Counters).
twice(Counters) -> get_keys_with_exact_count(2, Counters).

collect_from_beams([], Acc) -> Acc;
collect_from_beams([Beam|Beams], Acc) ->
    try
        {ok, Forms} = forms_from_beam(Beam),
        collect_atoms(Forms, Acc)
    of
        AccOut -> collect_from_beams(Beams, AccOut)
    catch
        error:E ->
            io:format(standard_error, "~s failed: ~p~n", [Beam, E]),
            collect_from_beams(Beams, Acc)
    end.

collect_atoms(Forms, Acc) ->
    ast_fold(fun inc_atoms/2, Acc, Forms).

inc_atoms({atom, _, Atom},                       Acc) -> inc(Atom, Acc);
%inc_atoms({attribute, _, module, Mod},           Acc) -> inc(Mod, Acc);
%inc_atoms({function, _, Func, _Arity, _Clauses}, Acc) -> inc(Func, Acc);
%inc_atoms({user_type, _, UserType, _Params},     Acc) -> inc(UserType, Acc);
inc_atoms(_,                                     Acc) -> Acc.

%% -----------------------------------------------
%% Counter implementation
new_counter_set() -> #{}.
inc(Key, Map) ->
    case Map of
        #{Key := Val} -> Map#{Key := Val + 1};
        #{}           -> Map#{Key => 1}
    end.

get_keys_with_exact_count(N, Map) ->
    maps:fold(fun (Key, Val, Acc) when Val == N ->
                      [Key|Acc];
                  (_, _, Acc) ->
                      Acc
              end, [], Map).

%% -----------------------------------------------

%% Folds an abstract syntax tree using a fun. The fun is applied to the
%% tuples in the tree in left-to-right depth-first pre-order.
-spec ast_fold(fun((_, Acc) -> Acc), Acc, Ast :: tuple()|list()) -> Acc.
ast_fold(_Fun, Acc, []) -> Acc;
ast_fold(Fun, Acc, [X|Xs]) ->
    Acc1 = ast_fold(Fun, Acc, X),
    ast_fold(Fun, Acc1, Xs);
ast_fold(Fun, Acc, Ast) when is_tuple(Ast) ->
    Acc1 = Fun(Ast, Acc),
    [_TagX, _AnnoX | Xs] = tuple_to_list(Ast),
    ast_fold(Fun, Acc1, Xs);
ast_fold(_Fun, Acc, _Leaf) ->
    Acc.

%% Load AST from beam file, compiled with debug_info.
forms_from_beam(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, Forms};
        {ok, {_Module, [{abstract_code,no_abstract_code}]}} ->
            {error, no_abstract_code};
        {error, _, _} = Error ->
            Error
    end.
