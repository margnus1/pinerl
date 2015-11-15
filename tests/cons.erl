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
