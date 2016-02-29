-module(mos_like_rabbit_cluster_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	mos_like_rabbit_cluster_sup:start_link().

stop(_State) ->
	ok.
