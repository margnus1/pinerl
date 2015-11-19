-module(pinerl).

-export([module/1, module/2, module/3
	,format_error/1
	]).

module(AbsForm) -> analyze(AbsForm, []).
module(AbsForm, FileName) -> module(AbsForm, FileName, []).
module(AbsForm, FileName, CompileOptions) ->
    analyze([{attribute,1,file,FileName}|AbsForm], CompileOptions).

analyze(AbsForm, Options) ->
    File = case AbsForm of
	       [{attribute,_,file,F}|_] -> F;
	       _ -> nofile
	   end,
    case catch pinerl_transform:parse_transform(AbsForm, Options) of
	{error,Es,Ws} -> {error,Es,Ws};
	{'EXIT',R} ->
	    {error,[{File,{none,compile,
			   {parse_transform,pinerl_transform,R}}}]};
	{warning, _Fs, Ws} -> {ok, Ws};
	_Fs -> {ok, []}
    end.

format_error({match_without_pin, V}) ->
    io_lib:format("variable ~p matched without pin", [V]);
format_error({undefined_var, V}) ->
    io_lib:format("variable ~p is unbound in pin", [V]).
