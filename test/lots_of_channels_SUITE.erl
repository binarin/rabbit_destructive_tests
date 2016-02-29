-module(lots_of_channels_SUITE).

-export([all/0]).

-export([list_channels_should_be_fast/1]).

all() ->
    [list_channels_should_be_fast].

list_channels_should_be_fast(_Config) ->
    ok.
