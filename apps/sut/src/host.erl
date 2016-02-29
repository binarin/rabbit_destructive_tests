-module(host).
-export([download/3]).

http_get(URL) ->
    {ok, 200, _RespHeaders, ClientRef} = hackney:request(get, URL, [], [], []),
    {ok, Body} = hackney:body(ClientRef),
    Body.

download(URL, TargetFile, Sut) ->
    file:write_file(sut:cache_dir(Sut) ++ "/" ++ TargetFile, http_get(URL)).
