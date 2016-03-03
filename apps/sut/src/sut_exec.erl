-module(sut_exec).
-export([run/1
        ,run/2
        ]).


-spec run(Cmd :: string()) -> integer().
run(Cmd) ->
    run(Cmd, []).

-spec run(Cmd :: string(), Env :: [{string(), string()}]) -> integer().
run([[_|_] = Cmd | Args], Env) ->
    Port = open_port({spawn_executable, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}, {args, stringify_args(Args)}]),
    run_loop([], Port);
run(Cmd, Env) ->
    Port = open_port({spawn, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}]),
    run_loop([], Port).

stringify_args([]) ->
    [];
stringify_args([Int|Rest]) when is_integer(Int) ->
    [integer_to_list(Int)|stringify_args(Rest)];
stringify_args([Something|Rest]) ->
    [Something|stringify_args(Rest)].

run_loop(Prefix, Port) ->
    receive
        {Port, {data, {Flag, Data}}} when  Flag =:= eol orelse Flag =:= noeol ->
            file:write(standard_io, [Prefix, Data, "\n"]),
            run_loop(Prefix, Port);
        {Port, {data, Data}} ->
            file:write(standard_io, [Prefix, Data]),
            run_loop(Prefix, Port);
        {Port, {exit_status, Status}} ->
            Status;
        {Port, _} ->
            run_loop(Prefix, Port)
    end.
