-module(tuple).

-include("pinerl.hrl").

-compile([export_all]).

legal1() ->
    {X = x, ?PIN(X) = x}.

illegal1() ->
    {X = x, X = x}.
