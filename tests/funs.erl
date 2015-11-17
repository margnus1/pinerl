-module(funs).

-include("pinerl.hrl").

-export([legal1/0, legal2/0, legal3/0, legal4/0, legal5/0
	,illegal1/0, illegal2/0, illegal3/0
	]).

-compile([nowarn_shadow_vars]).

legal1() ->
    _A = x,
    fun (_A) -> _A end.

legal2() ->
    A = x,
    fun(_B) -> ?PIN(A) = x end.

legal3() ->
    Var = x,
    _ =
	fun Var(0) -> 0;
	    Var(Y) -> 1 + Var(Y-1)
	end,
    ?PIN(Var) = x.

legal4() ->
    %% The fun name is shadowed in the first clause only.
    X =
    fun Var(Var) when Var =:= 0 -> 0;
	Var(Y) -> 1 + Var(Y-1)
    end,
    Var = X(3),
    Var.

legal5() ->
    fun Var(0) -> ?PIN(Var) = Var, 0;
	Var(Var) -> 1 + Var(Var-1)
    end.

%% To be made legal later
illegal1() ->
    _A = x,
    fun (?PIN(_A)) -> _A end.

illegal2() ->
    A = x,
    fun(_B) -> A = x end.

illegal3() ->
    fun Var(0) -> Var = Var, 0;
	Var(Var) -> 1 + Var(Var-1)
    end.
