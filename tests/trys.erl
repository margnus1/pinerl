-module(trys).

-include("pinerl.hrl").

-export([legal1/0]).

legal1() ->
    try _A = 2 of
	(_E = 2) -> _D = 2
    catch _:_ -> _E = 3
    end.
