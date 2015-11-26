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
-module(pinerl_transform).

-export([parse_transform/2]).

%% We can't dogfood by default or we get bootstrapping issues.
-ifdef(pinerl_transform_dogfood).
-include("pinerl.hrl").
-else.
-define(PIN(X),X).
-endif.

-define(WARNING_MODULE, pinerl).

-type defs() :: {ordsets:set(atom()),ordsets:set(atom())}.
-type warns() :: [{integer(),?WARNING_MODULE,term()}].
-type file() :: {string(), integer()}.
-type file_warns() :: [{file(),warns()}].
-type forms() :: [erl_parse:abstract_form()].

-spec parse_transform(forms(), list()) ->
			     forms() | {warning,forms(),file_warns()}.
parse_transform(Forms0, _Options) ->
    case forms(Forms0, [], nofile, []) of
	{Forms1, Warns} ->
	    Forms2 = insert_transformed_attribute(Forms1),
	    case Warns of
		[] ->  Forms2;
		_ -> {warning, Forms2, Warns}
	    end;
	already_processed ->
	    Forms0
    end.

insert_transformed_attribute([{attribute, Line, module, _}=E|Rest]) ->
    [E,{attribute,Line,pinerl_transformed,nil}|Rest];
insert_transformed_attribute([Form|Forms]) ->
    [Form|insert_transformed_attribute(Forms)].

-spec forms([Form], warns(), nofile|file(), [Form]) ->
		   {[Form], file_warns()} | already_processed.

forms([{attribute,_Line,pinerl_transformed,_}|_],_W,_F,_Acc) ->
    %% Avoid bad warnings if the parse transform is applied twice
    already_processed;
forms([{attribute,Line,file,File}|Fs],W,_F,Acc) ->	%This is valid anywhere.
    forms(Fs, W, File, [{attribute,Line,file,File}|Acc]);
forms([{function,Line,Name0,Arity0,Clauses0}|Fs], W0, File, Acc) ->
    {{Name,Arity,Clauses}, FW} = function(Name0, Arity0, Clauses0, []),
    W1 = case FW of
	     [] -> W0;
	     _ ->
		 %% Since rebar reimplements the compiler, it does not accept
		 %% all the formats the real compiler does. If we include the
		 %% inclusion-point, rebar will crash with a cryptic error when
		 %% a warning is emitted. Sigh...
		 FileName = case File of
				{FileName0, _FileInclusionLine} -> FileName0;
				FileName0
				  when is_list(FileName0); is_atom(FileName0);
				       is_binary(FileName0) ->
				    FileName0
			    end,
		 [{FileName,FW}|W0]
	 end,
    forms(Fs, W1, File, [{function,Line,Name,Arity,Clauses}|Acc]);
forms([Other|Fs], W, F, Acc) -> forms(Fs, W, F, [Other|Acc]);
forms([], Warns, _File, Forms) ->
    {lists:reverse(Forms), lists:reverse(Warns)}.

-spec function(atom(), integer(), [Clause], warns()) -> 
		      {{atom(),integer(),[Clause]}, warns()}.

function(Name, Arity, Clauses0, W0) ->
    {Clauses1, W1} = clauses(Clauses0, W0),
    {{Name,Arity,Clauses1}, W1}.

-spec clauses([Clause], warns()) -> {[Clause], warns()}.

clauses([C0|Cs0],W0) ->
    {C1, _D1, W1} = clause(C0, {ordsets:new(),ordsets:new()}, W0),
    {Cs1, W2} = clauses(Cs0, W1),
    {[C1|Cs1], W2};
clauses([], W) -> {[], W}.

-spec clause(Clause,defs(),warns()) -> {Clause, defs(), warns()}.

clause({clause,Line,H0,G,B0},D0,W0) ->
    {H1,D1,W1} = head(H0,D0,W0),
    %% No more shadowing after the head
    D2 = defs_end_shadowing(D1),
    {B1,D3,W2} = exprs(B0,D2,W1),
    {{clause,Line,H1,G,B1}, D3, W2}.

-spec head([Pattern],defs(),warns()) -> {[Pattern],defs(),warns()}.

head(Ps,D,W) -> patterns(Ps,D,W).

-spec patterns([Pattern],defs(),warns()) -> {[Pattern],defs(),warns()}.
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps0],D0,W0) ->
    {P1, D1, W1} = pattern(P0, D0, W0),
    {Ps1, D2, W2} = patterns(Ps0, D1, W1),
    {[P1|Ps1], D2, W2};
patterns([],D,W) -> {[],D,W}.

-spec pattern(Pattern, defs(), warns()) -> {Pattern, defs(), warns()}.

pattern({var,Line,'_'},D,W) ->
    {{var,Line,'_'},D,W};
pattern({var,Line,V},D={_S,Defs},W0) ->
    %% TODO: THE MAGIC HAPPENS HERE
    W1 = case ordsets:is_element(V,Defs) of
	     false -> W0;
	     true -> [{Line, ?WARNING_MODULE, {match_without_pin, V}} | W0]
	 end,
    {{var,Line,V}, defs_add(V,D), W1};
pattern({match,Line,L0,R0},D0,W0) ->
    {L1,D1,W1} = pattern(L0,D0,W0),
    {R1,D2,W2} = pattern(R0,D1,W1),
    {{match,Line,L1,R1}, D2, W2};
pattern({cons,Line,H0,T0},D0,W0) ->
    {H1,D1,W1} = pattern(H0,D0,W0),
    {T1,D2,W2} = pattern(T0,D1,W1),
    {{cons,Line,H1,T1}, D2, W2};
pattern({tuple,Line,Ps0},D0,W0) ->
    {Ps1,D1,W1} = pattern_list(Ps0,D0,W0),
    {{tuple,Line,Ps1},D1,W1};
pattern({map,Line,Ps0},D0,W0) ->
    {Ps1,D1,W1} = pattern_list(Ps0,D0,W0),
    {{map,Line,Ps1},D1,W1};
pattern({map_field_exact,Line,K,V},D0,W0) ->
    {Ke,D1,W1} = expr(K,D0,W0),
    {Ve,D2,W2} = pattern(V,D1,W1),
    {{map_field_exact,Line,Ke,Ve},D2,W2};
pattern({record,Line,Name,Pfs0},D0,W0) ->
    {Pfs1,D1,W1} = pattern_fields(Pfs0,D0,W0),
    {{record,Line,Name,Pfs1},D1,W1};
pattern({record_index,Line,Name,Field0},D0,W0) ->
    {Field1,D1,W1} = pattern(Field0,D0,W0),
    {{record_index,Line,Name,Field1},D1,W1};
pattern({record_field,Line,Rec0,Name,Field0},D0,W0) ->
    {Rec1,D1,W1} = expr(Rec0,D0,W0),
    {Field1,D2,W2} = expr(Field0,D1,W1),
    {{record_field,Line,Rec1,Name,Field1},D2,W2};
pattern({record_field,Line,Rec0,Field0},D0,W0) ->
    {Rec1,D1,W1} = expr(Rec0,D0,W0),
    {Field1,D2,W2} = expr(Field0,D1,W1),
    {{record_field,Line,Rec1,Field1},D2,W2};
pattern({bin,Line,Fs},D0,W0) ->
    {Fs2,D1,W1} = pattern_grp(Fs,D0,W0),
    {{bin,Line,Fs2},D1,W1};
pattern({block,_BLine,[{var,VLine,V}]},D={_S,Defs},W0) ->
    %% TODO: THE MAGIC HAPPENS HERE
    W1 = case ordsets:is_element(V,Defs) of
	     true -> W0;
	     false -> 
		 %% case ordsets:is_element(V,S) of
		 %%     true -> %% TODO: EXTRA FEATURE
		 [{VLine, ?WARNING_MODULE, {undefined_var, V}} | W0]
	 end,
    %% Insert anyway to not generate spurious extra warnings
    {{var, VLine, V}, defs_add(V, D), W1};
pattern(Literal,D,W) -> {Literal,D,W}.


-spec pattern_grp([PGrp], defs(), warns()) -> {[PGrp], defs(), warns()}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs0],D0,W0) ->
    {S2,D1,W1} = case S1 of
	     default ->
		 {default,D0,W0};
	     _ ->
		 expr(S1,D0,W0)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    {E2,D2,W2} = expr(E1,D1,W1),
    {Fs1,D3,W3} = pattern_grp(Fs0,D2,W2),
    {[{bin_element,L1,E2,S2,T2} | Fs1],D3,W3};
pattern_grp([],D,W) ->
    {[],D,W}.

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].



-spec pattern_list([Pattern],defs(),warns()) -> {[Pattern],defs(),warns()}.
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps0],D0,W0) ->
    {P1,D1,W1} = pattern(P0,D0,W0),
    {Ps1,D2,W2} = pattern_list(Ps0,D1,W1),
    {[P1|Ps1],D2,W2};
pattern_list([],D,W) -> {[],D,W}.

-spec pattern_fields([Field],defs(),warns()) -> {[Field],defs(),warns()}.
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs0],D0,W0) ->
    {P1,D1,W1} = pattern(P0,D0,W0),
    {Pfs1,D2,W2} = pattern_fields(Pfs0,D1,W1),
    {[{record_field,Lf,{atom,La,F},P1}|Pfs1],D2,W2};
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs0],D0,W0) ->
    {P1,D1,W1} = pattern(P0,D0,W0),
    {Pfs1,D2,W2} = pattern_fields(Pfs0,D1,W1),
    {[{record_field,Lf,{var,La,'_'},P1}|Pfs1],D2,W2};
pattern_fields([],D,W) -> {[],D,W}.

-spec exprs([Expression], defs(), warns()) -> {[Expression], defs(), warns()}.
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es],D0,W0) ->
    {E1,D1,W1} = expr(E0,D0,W0),
    {Es1,D2,W2} = exprs(Es,D1,W1),
    {[E1|Es1],D2,W2};
exprs([],D,W) -> {[],D,W}.

-spec expr(Expression,defs(),warns()) -> {Expression,defs(),warns()}.

expr({cons,Line,H0,T0},D0,W0) ->
    {H1,D1,W1} = expr(H0,D0,W0),
    {T1,D2,W2} = expr(T0,D1,W1),				%They see the same variables
    {{cons,Line,H1,T1},D2,W2};
expr({lc,Line,E0,Qs0},D0,W0) ->
    {Qs1,D1,W1} = lc_bc_quals(Qs0,D0,W0),
    {E1,_D2,W2} = expr(E0,D1,W1),
    %% Definitions in qualifier or body does not leak
    {{lc,Line,E1,Qs1},D0,W2};
expr({bc,Line,E0,Qs0},D0,W0) ->
    {Qs1,D1,W1} = lc_bc_quals(Qs0,D0,W0),
    {E1,_D2,W2} = expr(E0,D1,W1),
    %% Definitions in qualifier or body does not leak
    {{bc,Line,E1,Qs1},D0,W2};
expr({tuple,Line,Es0},D0,W0) ->
    {Es1,D1,W1} = expr_list(Es0,D0,W0),
    {{tuple,Line,Es1},D1,W1};
expr({map,Line,Map0,Es0},D0,W0) ->
    {[Map1|Es1],D1,W1} = exprs([Map0|Es0],D0,W0),
    {{map,Line,Map1,Es1},D1,W1};
expr({map,Line,Es0},D0,W0) ->
    {Es1,D1,W1} = exprs(Es0,D0,W0),
    {{map,Line,Es1},D1,W1};
expr({map_field_assoc,Line,K,V},D0,W0) ->
    {Ke,D1,W1} = expr(K,D0,W0),
    {Ve,D2,W2} = expr(V,D1,W1),
    {{map_field_assoc,Line,Ke,Ve},D2,W2};
expr({map_field_exact,Line,K,V},D0,W0) ->
    {Ke,D1,W1} = expr(K,D0,W0),
    {Ve,D2,W2} = expr(V,D1,W1),
    {{map_field_exact,Line,Ke,Ve},D2,W2} ;
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0},D0,W0) ->
    {Field1,D1,W1} = expr(Field0,D0,W0),
    {{record_index,Line,Name,Field1},D1,W1};
expr({record,Line,Name,Inits0},D0,W0) ->
    {Inits1,D1,W1} = record_inits(Inits0,D0,W0),
    {{record,Line,Name,Inits1},D1,W1};
expr({record_field,Line,Rec0,Name,Field0},D0,W0) ->
    {Rec1,D1,W1} = expr(Rec0,D0,W0),
    {Field1,D2,W2} = expr(Field0,D1,W1),
    {{record_field,Line,Rec1,Name,Field1},D2,W2};
expr({record,Line,Rec0,Name,Upds0},D0,W0) ->
    {Rec1,D1,W1} = expr(Rec0,D0,W0),
    {Upds1,D2,W2} = record_updates(Upds0,D1,W1),
    {{record,Line,Rec1,Name,Upds1},D2,W2};
expr({record_field,Line,Rec0,Field0},D0,W0) ->
    {Rec1,D1,W1} = expr(Rec0,D0,W0),
    {Field1,D2,W2} = expr(Field0,D1,W1),
    {{record_field,Line,Rec1,Field1},D2,W2};
expr({block,_BLine,[{var,VLine,V}]},D,W) ->			
    %% TODO: Warn about spurious pin?
    {{block,_BLine,[{var,VLine,V}]},D,W};
expr({block,Line,Es0},D0,W0) ->
    %% Unfold block into a sequence.
    {Es1,D1,W1} = exprs(Es0,D0,W0),
    {{block,Line,Es1},D1,W1};
expr({'if',Line,Cs0},D0,W0) ->
    {Cs1,D1,W1} = icr_clauses(Cs0,D0,W0),
    {{'if',Line,Cs1},D1,W1};
expr({'case',Line,E0,Cs0},D0,W0) ->
    {E1,D1,W1} = expr(E0,D0,W0),
    {Cs1,D2,W2} = icr_clauses(Cs0,D1,W1),
    {{'case',Line,E1,Cs1},D2,W2};
expr({'receive',Line,Cs0},D0,W0) ->
    {Cs1,D1,W1} = icr_clauses(Cs0,D0,W0),
    {{'receive',Line,Cs1},D1,W1};
expr({'receive',Line,Cs0,To0,ToEs0},D0,W0) ->
    {To1,D1,W1} = expr(To0,D0,W0),
    {ToEs1,D2,W2} = exprs(ToEs0,D1,W1),
    {Cs1,D3,W3} = icr_clauses(Cs0,D1,W2),
    {{'receive',Line,Cs1,To1,ToEs1},defs_inf(D2, D3),W3};
expr({'try',Line,Es0,Scs0,Ccs0,As0},D0,W0) ->
    {Es1,_D1,W1} = exprs(Es0,D0,W0),
    %% Odd that these do not see D1
    {Scs1,_D2,W2} = icr_clauses(Scs0,D0,W1),
    {Ccs1,_D3,W3} = icr_clauses(Ccs0,D0,W2),
    {As1,_D4,W4} = exprs(As0,D0,W3),
    {{'try',Line,Es1,Scs1,Ccs1,As1}, D0, W4};
expr({'fun',Line,Body},D0,W0) ->
    case Body of
	{clauses,Cs0} ->
	    {Cs1,W1} = fun_clauses(Cs0,D0,W0),
	    {{'fun',Line,{clauses,Cs1}},D0,W1};
	{function,F,A} ->
	    {{'fun',Line,{function,F,A}},D0,W0};
	{function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    %% R10B-6: fun M:F/A. (Backward compatibility)
	    {{'fun',Line,{function,M,F,A}},D0,W0};
	{function,M0,F0,A0} ->
	    %% R15: fun M:F/A with variables.
	    {M,D1,W1} = expr(M0,D0,W0),
	    {F,D2,W2} = expr(F0,D1,W1),
	    {A,D3,W3} = expr(A0,D2,W2),
	    {{'fun',Line,{function,M,F,A}},D3,W3}
    end;
expr({named_fun,Loc,Name,Cs0},D0,W0) ->
    D1 = defs_add(Name, D0),
    {Cs1,W1} = fun_clauses(Cs0,D1,W0),
    {{named_fun,Loc,Name,Cs1},D0,W1};
expr({call,Line,F0,As0},D0,W0) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    {F1,D1,W1} = expr(F0,D0,W0),
    {As1,D2,W2} = expr_list(As0,D1,W1),
    {{call,Line,F1,As1},D2,W2};
expr({'catch',Line,E0},D0,W0) ->
    %% No new variables added.
    {E1,D1,W1} = expr(E0,D0,W0),
    %% TODO: Isn't any new defs in D1 "unsafe"? Do we keep them?
    {{'catch',Line,E1},D1,W1};
expr({match,Line,P0,E0},D0,W0) ->
    {E1,D1,W1} = expr(E0,D0,W0),
    {P1,D2,W2} = pattern(P0,D1,W1),
    {{match,Line,P1,E1},D2,W2};
expr({bin,Line,Fs},D0,W0) ->
    {Fs2,D1,W1} = pattern_grp(Fs,D0,W0),
    {{bin,Line,Fs2},D1,W1};
expr({op,Line,Op,A0},D0,W0) ->
    {A1,D1,W1} = expr(A0,D0,W0),
    {{op,Line,Op,A1},D1,W1};
expr({op,Line,Op,L0,R0},D0,W0) ->
    %% TODO: What is this ?!
    {L1,D1,W1} = expr(L0,D0,W0),
    {R1,D2,W2} = expr(R0,D1,W1),				%They see the same variables
    {{op,Line,Op,L1,R1},D2,W2};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0},D0,W0) ->
    {M1,D1,W1} = expr(M0,D0,W0),
    {F1,D2,W2} = expr(F0,D1,W1),
    {{remote,Line,M1,F1},D2,W2};
expr(Literal,D,W) -> {Literal,D,W}.


-spec expr_list([Expression],defs(),warns()) -> {[Expression],defs(),warns()}.
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es0],D0,W0) ->
    %% TODO: deal with "in parallel" thingy
    {E1,D1,W1} = expr(E0,D0,W0),
    {Es1,D2,W2} = expr_list(Es0,D1,W1),
    {[E1|Es1],D2,W2};
expr_list([],D,W) -> {[],D,W}.

-spec record_inits([RecordInit],defs(),warns()) -> {[RecordInit],defs(),warns()}.
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is0],D0,W0) ->
    {Val1,D1,W1} = expr(Val0,D0,W0),
    {Is1,D2,W2} = record_inits(Is0,D1,W1),
    {[{record_field,Lf,{atom,La,F},Val1}|Is1],D2,W2};
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is0],D0,W0) ->
    {Val1,D1,W1} = expr(Val0,D0,W0),
    {Is1,D2,W2} = record_inits(Is0,D1,W1),
    {[{record_field,Lf,{var,La,'_'},Val1}|Is1],D2,W2};
record_inits([],D,W) -> {[],D,W}.

-spec record_updates([RecordUpd],defs(),warns()) -> {[RecordUpd],defs(),warns()}.
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us0],D0,W0) ->
    {Val1,D1,W1} = expr(Val0,D0,W0),
    {Us1,D2,W2} = record_updates(Us0,D1,W1),
    {[{record_field,Lf,{atom,La,F},Val1}|Us1],D2,W2};
record_updates([],D,W) -> {[],D,W}.

-spec icr_clauses([Clause],defs(),warns()) -> {[Clause],defs(),warns()}.

icr_clauses([C0],D0,W0) ->
    {C1,D1,W1} = clause(C0,D0,W0),
    {[C1],D1,W1};
icr_clauses([C0|Cs0],D0,W0) ->
    {C1,D1,W1} = clause(C0,D0,W0),
    {Cs1,D2,W2} = icr_clauses(Cs0,D0,W1),
    {[C1|Cs1], defs_inf(D1,D2), W2};
icr_clauses([],D,W) -> {[],D,W}.

-spec lc_bc_quals([Qualifier],defs(),warns()) -> {[Qualifier],defs(),warns()}.
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs0],D0,W0) ->
    {E1,_D1,W1} = expr(E0,D0,W0),
    {P1,D2,W2} = pattern(P0,defs_start_shadowing(D0),W1),
    {Qs1,D3,W3} = lc_bc_quals(Qs0,defs_end_shadowing(D2),W2),
    {[{generate,Line,P1,E1}|Qs1],D3,W3};
lc_bc_quals([{b_generate,Line,P0,E0}|Qs0],D0,W0) ->
    {E1,_D1,W1} = expr(E0,D0,W0),
    {P1,D2,W2} = pattern(P0,defs_start_shadowing(D0),W1),
    {Qs1,D3,W3} = lc_bc_quals(Qs0,defs_end_shadowing(D2),W2),
    {[{b_generate,Line,P1,E1}|Qs1],D3,W3};
lc_bc_quals([E0|Qs0],D0,W0) ->
    {E1,D1,W1} = expr(E0,D0,W0),
    {Qs1,D2,W2} = lc_bc_quals(Qs0,D1,W1),
    {[E1|Qs1],D2,W2};
lc_bc_quals([],D,W) -> {[],D,W}.

-spec fun_clauses([Clause],defs(),warns()) -> {[Clause],warns()}.

fun_clauses([C0|Cs0],D,W0) ->
    {C1,_D1,W1} = clause(C0,defs_start_shadowing(D),W0),
    {Cs1,W2} = fun_clauses(Cs0,D,W1),
    {[C1|Cs1], W2};
fun_clauses([],_D,W) -> {[],W}.

%% UTILITIES ------------------------------------------------------------------

%% Computes the infinimum (or intersection) of two definition maps.
-spec defs_inf(defs(), defs()) -> defs().
defs_inf({SA,DA},{SB,DB}) ->
    ?PIN(SA) = SB,
    {SA, ordsets:intersection(DA, DB)}.

-spec defs_add(atom(), defs()) -> defs().
defs_add(Var, {S,Defs}) ->
    {S,ordsets:add_element(Var,Defs)}.

-spec defs_start_shadowing(defs()) -> defs().
defs_start_shadowing({S,Defs}) ->
    {0, _} = {ordsets:size(S), S},
    {Defs, ordsets:new()}.

-spec defs_end_shadowing(defs()) -> defs().
defs_end_shadowing({S,Defs}) ->
    {ordsets:new(), ordsets:union(S, Defs)}.
