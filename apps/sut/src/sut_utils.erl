-module(sut_utils).
-export([random/1]).

-spec random(non_neg_integer()) -> non_neg_integer().

random(N) ->
    case get(random_seed) of
        undefined ->
            random:seed(erlang:phash2([node()]),
                        erlang:monotonic_time(),
                        erlang:unique_integer());
        _ -> ok
    end,
    random:uniform(N).
