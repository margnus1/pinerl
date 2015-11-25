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
-module(cons).

-include("pinerl.hrl").

-export([legal1/0, illegal1/0]).

%% The reason for this testcase is that the erlang compiler treats conses very
%% strangely: the expression [A=1|A] does not compile with the message 'A' is
%% unbound. Same for [A|A=1]. [A=1|A=2] is fine but causes a match failure at
%% runtime.

%% Is it even... /legal/?
%% I'll make it... "legal"!
legal1() ->
    _=[A=1 | ?PIN(A)=1],
    ok.

illegal1() ->
    _=[A=1 | A=1],
    ok.
