-module(sut_exec).
-export([system/1
        ,system/2
        ,run/2
        ,run/3
        ]).

-type options() :: [].

%% @equiv run(Cmd, [], Options)
run(Cmd, Options) ->
    run(Cmd, [], Options).

%% @doc Runs given command. Returns exit code and stdout/stderr as binary.
-spec run(Cmd :: string(), Args :: [string()], options()) -> RunResult when
      RunResult :: {ExitCode :: integer(), StdOutAndErr :: binary()}.

run(Cmd, Args, Options) ->
    Port = open_run_port(Cmd, Args, Options),
    run_loop(Port, Options, run_loop_empty_accumulator(Options)).

run_loop_empty_accumulator(Options) ->
    case proplists:get_value(line, Options) of
        undefined ->
            <<>>;
        _ ->
            []
    end.

run_to_port_options([]) ->
    [];
run_to_port_options([{env, _} = Opt | Rest]) ->
    [Opt | run_to_port_options(Rest)];
run_to_port_options([line | Rest]) ->
    [{line, 10000} | run_to_port_options(Rest)];
run_to_port_options([_Opt | Rest]) ->
    run_to_port_options(Rest).

open_run_port(Cmd, Args, Options) ->
    PortOptions = [{args, stringify_args(Args)}, exit_status, stderr_to_stdout, binary]
        ++ run_to_port_options(Options),
    open_port({spawn_executable, os:find_executable(Cmd)}, PortOptions).

run_loop(Port, Options, StdOutAndErr) ->
    receive
        {Port, {data, {eol, Data}}} ->
            run_loop(Port, Options, [Data|StdOutAndErr]);
        {Port, {data, {noeol, Data}}} ->
            [Current|Rest] = StdOutAndErr,
            run_loop(Port, Options, [<<Current/binary, Data/binary>> | Rest]);
        {Port, {data, Data}} ->
            run_loop(Port, Options, <<StdOutAndErr/binary, Data/binary>>);
        {Port, {exit_status, Status}} ->
            case StdOutAndErr of
                Binary when is_binary(Binary) ->
                    {Status, Binary};
                List when is_list(List) ->
                    {Status, lists:reverse(List)}
            end;
        {Port, Unhandled} ->
            exit({unhandled, Port, Unhandled})
    end.

%% @doc Run given command and stream its output to
%% standard_io. Returns exit code of that program.
-spec system(Cmd :: string()) -> integer().
system(Cmd) ->
    system(Cmd, []).

-spec system(Cmd :: string(), Env :: [{string(), string()}]) -> integer().
system([[_|_] = Cmd | Args], Env) ->
    Port = open_port({spawn_executable, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}, {args, stringify_args(Args)}]),
    system_loop([], Port);
system(Cmd, Env) ->
    Port = open_port({spawn, Cmd},
                     [{env, Env}, stderr_to_stdout, exit_status, {line, 10000}]),
    system_loop([], Port).

stringify_args([]) ->
    [];
stringify_args([Int|Rest]) when is_integer(Int) ->
    [integer_to_list(Int)|stringify_args(Rest)];
stringify_args([Something|Rest]) ->
    [Something|stringify_args(Rest)].

system_loop(Prefix, Port) ->
    receive
        {Port, {data, {Flag, Data}}} when  Flag =:= eol orelse Flag =:= noeol ->
            file:write(standard_io, [Prefix, Data, "\n"]),
            system_loop(Prefix, Port);
        {Port, {data, Data}} ->
            file:write(standard_io, [Prefix, Data]),
            system_loop(Prefix, Port);
        {Port, {exit_status, Status}} ->
            Status;
        {Port, _} ->
            system_loop(Prefix, Port)
    end.
