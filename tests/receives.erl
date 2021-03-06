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
-module(receives).

-include("pinerl.hrl").

-export([legal1/0, legal2/0, legal3/0
	,illegal1/0, illegal2/0, illegal3/0, illegal4/0
	]).

legal1() ->
    receive _ -> ?PIN(_A) = 1000
    after (_A = 1000) -> timeout
    end.

legal2() ->
    receive _ -> {no, _A = timeout}
    after 1000 -> (_A = timeout)
    end.

legal3() ->
    receive x -> A = timex;
	    _ -> A = timein
    after 1000 -> A = timeout
    end,
    ?PIN(A) = timeout,
    ok.

illegal1() ->
    receive _ -> _A = 1000
    after (_A = 1000) -> timeout
    end.

illegal2() ->
    receive _ -> {no, ?PIN(_A) = timeout}
    after 1000 -> (_A = timeout)
    end.

illegal3() ->
    receive _ -> {no, _A = timeout}
    after 1000 -> (?PIN(_A) = timeout)
    end.

illegal4() ->
    receive x -> A = timex;
	    _ -> A = timein
    after 1000 -> A = timeout
    end,
    A = timeout,
    ok.
