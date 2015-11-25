%% Copyright 2015 Magnus Lång
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
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
