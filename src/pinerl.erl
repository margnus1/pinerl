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
-module(pinerl).

-export([module/1, module/2, module/3
	,format_error/1
	]).

-type error_info() :: {none|erl_anno:line(), module(), term()}.
-type errors() :: [{file:filename(), [error_info()]}].
-type forms() :: [erl_parse:abstract_form()].
-type options() :: [compile:option()].
-type ret() :: {ok, errors()} | {error, errors(), errors()}.

-spec module(forms()) -> ret().
-spec module(forms(), file:filename()) -> ret().
-spec module(forms(), file:filename(), options()) -> ret().
module(AbsForm) -> analyze(AbsForm, []).
module(AbsForm, FileName) -> module(AbsForm, FileName, []).
module(AbsForm, FileName, CompileOptions) ->
    analyze([{attribute,1,file,FileName}|AbsForm], CompileOptions).

-spec analyze(forms(), options()) -> ret().
analyze(AbsForm, Options) ->
    File = case AbsForm of
	       [{attribute,_,file,F}|_] -> F;
	       _ -> nofile
	   end,
    case catch pinerl_transform:parse_transform(AbsForm, Options) of
	{error,Es,Ws} -> {error,Es,Ws};
	{'EXIT',R} ->
	    {error,[{File,[{none,compile,
			    {parse_transform,pinerl_transform,R}}]}], []};
	{warning, _Fs, Ws} -> {ok, Ws};
	_Fs -> {ok, []}
    end.

format_error({match_without_pin, V}) ->
    io_lib:format("variable ~p matched without pin", [V]);
format_error({undefined_var, V}) ->
    io_lib:format("variable ~p is unbound in pin", [V]).
