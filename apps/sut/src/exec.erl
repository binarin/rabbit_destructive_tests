-module(exec).
-include("include/sut.hrl").

-type shell_cmd_simple() :: string().
-type shell_cmd_with_args() :: [string()].
-type shell_cmd() :: shell_cmd_simple() | shell_cmd_with_args().

-export_type([shell_cmd/0, shell_cmd_simple/0, shell_cmd_with_args/0]).

%% -spec parallel_run (shell_cmd(), sut:sut()) -> [integer()].

%% parallel_run(_Cmd, _Sut) ->
%%     [10].
