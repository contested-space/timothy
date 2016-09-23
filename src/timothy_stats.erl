-module(timothy_stats).

-export([
         mean/1
]).


mean(Samples) ->
    N = length(Samples),
    lists:foldl(
      fun(X, Avg) -> X/N + Avg end,
      0.0,
      Samples).
