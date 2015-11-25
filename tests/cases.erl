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
-module(cases).

-include("pinerl.hrl").
%%-define(PIN(X),X).

%% No need for this warning; with pinerl programmer intention is never ambigous.
-compile([nowarn_export_vars]).

-export([legal1/0, legal2/0, illegal1/0, illegal2/0, illegal3/0]).

legal1() ->
    case false of
	true -> A = 1;
	false -> A = 2
    end,
    ?PIN(A) = 2,
    A.

legal2() ->
    A = 2,
    case 2 of
	?PIN(A) -> ok;
	_Else -> bleh
    end.

illegal1() ->
    case false of
	true -> A = 1;
	false -> A = 2
    end,
    A = 2,
    A.

illegal2() ->
    case false of
	true -> A = 1;
	false -> ?PIN(A) = 2
    end,
    B = 2,
    {A,B}.

illegal3() ->
    case 2 of
	?PIN(_A) -> ok;
	_Else -> bleh
    end.
