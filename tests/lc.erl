-module(lc).

-include("pinerl.hrl").

-compile([export_all
	 ,nowarn_shadow_vars
	 ]).

legal1() ->
    _X = x,
    {[_X || {_X, _Y} <- [{x, y}, {y, x}]], _X}.

legal2() ->
    X = x,
    {[X || {K, _V} <- [{x, y}, {y, x}], ?PIN(X) = K]}.

legal3() ->
    X = x,
    {[K || {K, _V} <- [{?PIN(X)=x, y}, {y, x}]]}.

legal4() ->
    %% _X in the generator is not visible outside the generator!
    [_X = x
     || {_A, _B} <- [{_X=x, y}, {y, x}]].

legal5() ->
    [_X || {_X, _B} <- [{_X=x, y}, {y, x}]].

legal6() ->
    [K || {K, _B} <- [{_X=x, y}, {y, x}], _X = true].
    
legal7() ->
    [?PIN(X) = x
     || {A, _B} <- [{x, y}, {y, x}], (X=A) =/= thingy].

%% To be made legal
illegal1() ->    
    X = x,
    {[X || {?PIN(X), _Y} <- [{x, y}, {y, x}]], X}.

illegal2() ->
    X = x,
    {[X || {K, _V} <- [{x, y}, {y, x}], X = K]}.

illegal3() ->
    X = x,
    {[K || {K, _V} <- [{X=x, y}, {y, x}]]}.

illegal4() ->
    [?PIN(_X) = x
     || {_A, _B} <- [{_X=x, y}, {y, x}]].

illegal5() ->
    [_X || {?PIN(_X), _B} <- [{_X=x, y}, {y, x}]].

illegal6() ->
    [K || {K, _B} <- [{_X=x, y}, {y, x}], ?PIN(_X) = true].
    
illegal7() ->
    [X = x
     || {A, _B} <- [{x, y}, {y, x}], (X=A) =/= thingy].
