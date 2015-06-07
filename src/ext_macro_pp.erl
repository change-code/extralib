%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

-module(ext_macro_pp).

-export([open_file/3]).


open_file(FileName, Passes, Options) ->
    Pdm = proplists:get_value(macros, Options, []),
    case ext_epp:open_file(FileName, Passes, Options) of
        {ok, State, Cont} ->
            #{name := Name} = State,
            case user_predef(Pdm, predef_macros(Name)) of
                {ok, Macs} ->
                    {ok, State#{
                           name2 => Name,
                           delta => 0,
                           macs  => Macs,
                           uses  => dict:new(),
                           istk  => []},
                     fun (St) ->
                             {Rep, St1, Cont1} = Cont(St),
                             {Rep, St1, wait_req_scan(Cont1, Passes, Options)}
                     end};
                {error, _} = Error ->
                    Error
            end;
        Other ->
            Other
    end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File) ->
     Machine = list_to_atom(erlang:system_info(machine)),
     dict:from_list([
        {{atom,'FILE'},               {none,[{string,1,File}]}},
        {{atom,'LINE'},               {none,[{integer,1,1}]}},
        {{atom,'MODULE'},             undefined},
        {{atom,'MODULE_STRING'},      undefined},
        {{atom,'BASE_MODULE'},        undefined},
        {{atom,'BASE_MODULE_STRING'}, undefined},
        {{atom,'MACHINE'},            {none,[{atom,1,Machine}]}},
        {{atom,Machine},              {none,[{atom,1,true}]}}
     ]).

%% user_predef(PreDefMacros, Macros) ->
%%      {ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
    user_predef(Pdm, dict:store({atom,M}, {none,Exp}, Ms));
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
        {ok,_Defs} when is_list(_Defs) -> %% User defined macros
            {error,{redefine,M}};
        {ok,_Def} -> %% Predefined macros
            {error,{redefine_predef,M}};
        error ->
            Exp = erl_parse:tokens(erl_parse:abstract(Val)),
            user_predef(Pdm, dict:store({atom,M}, [{none, {none,Exp}}], Ms))
    end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
        {ok,_Defs} when is_list(_Defs) -> %% User defined macros
            {error,{redefine,M}};
        {ok,_Def} -> %% Predefined macros
            {error,{redefine_predef,M}};
        error ->
            user_predef(Pdm,
                        dict:store({atom,M}, [{none, {none,[{atom,1,true}]}}], Ms))
    end;
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_req_scan(Cont, Passes, Options) ->
    fun (State) ->
            scan_toks(Cont, State, Passes, Options)
    end.

wait_req_skip(Cont, Passes, Options, Sis) ->
    fun (State) ->
            skip_toks(Cont, State, Passes, Options, Sis)
    end.

%% enter_file(FileName, IncludeToken, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

path_open([], _, _, _) ->
    {error, not_found};
path_open([Path|Paths], FileName, Passes, Options) ->
    FullName = filename:join(Path, FileName),
    case ext_epp:open_file(FullName, Passes, Options) of
        {ok, State, Cont} ->
            {ok, State, Cont};
        {error, _} ->
            path_open(Paths, FileName, Passes, Options)
    end.

enter_file(NewName, Inc, Cont, St, Passes, Options) ->
    #{name:=FileName, location:=Loc} = St,
    Path = [filename:dirname(FileName)
            |proplists:get_value(includes, Options, [])],
    case path_open(Path, NewName, Passes, Options) of
        {ok, St1, Cont1} ->
            enter_file2(St1, Cont1, Cont, St, Passes, Options, start_loc(Loc));
        {error, _E} ->
            {{error,{abs_loc(Inc),epp,{include,file,NewName}}}, St, wait_req_scan(Cont, Passes, Options)}
    end.

%% enter_file2(File, FullName, From, EppState, AtLocation) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file2(St1, Cont1, Cont, St0, Passes, Options, AtLocation) ->
    #{name:=Pname} = St1,
    Loc = start_loc(AtLocation),
    Rep = ext_epp:enter_file_reply(Pname, Loc, AtLocation),
    #{macs:=Macs} = St0,
    Ms = dict:store({atom,'FILE'}, {none,[{string,Loc,Pname}]}, Macs),
    St2 = St1#{
      name2    => Pname,
      delta    => 0,
      parent   => {Cont, St0},
      macs     => Ms,
      istk     => [],
      uses     => dict:new()}, %% XXX
    {Rep, St2,
     fun(State) ->
	{_, State1, Cont2} = Cont1(State),
        scan_toks(Cont2, State1, Passes, Options)
     end}.

leave_file(Cont, St, Passes, Options) ->
    #{istk:=IStk, location:=Location, macs:=Macs, uses:=Uses} = St,
    case IStk of
        [I|Cis] ->
            {{error,{Location,epp,{illegal,"unterminated",I}}}, St,
             fun (State) ->
                     leave_file(Cont, State#{istk:=Cis}, Passes, Options)
             end};
        [] ->
            case St of
                #{parent:={OldCont, OldSt}} ->
                    #{location:=OldLoc, delta:=Delta, name:=OldName,
                      name2:=OldName2} = OldSt,
                    CurrLoc = add_line(OldLoc, Delta),
                    Ms = dict:store({atom,'FILE'},
                                    {none,[{string,CurrLoc,OldName2}]},
                                    Macs),
                    NextSt = OldSt#{macs:=Ms,uses:=Uses},
                    Rep = ext_epp:enter_file_reply(OldName, CurrLoc, CurrLoc),

                    case OldName2 =:= OldName of
                        true ->
                            {Rep, NextSt, wait_req_scan(OldCont, Passes, Options)};
                        false ->
                            {Rep, NextSt,
                             fun(State) ->
                                     Rep1 = ext_epp:enter_file_reply(OldName2, OldLoc, neg_line(CurrLoc)),
                                     {Rep1, State, wait_req_scan(OldCont, Passes, Options)}
                             end}
                    end;
                _ ->
                    {{eof,Location}, St, Cont}
            end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(Cont, State, Passes, Options) ->
    case Cont(State) of
        {{ok, Tokens}, State1, Cont1} ->
            scan_toks(Tokens, Cont1, State1, Passes, Options);
        {{eof, _}, State1, eof} ->
            leave_file(eof, State1, Passes, Options);
        {Other, State1, Cont1} ->
            {Other, State1, wait_req_scan(Cont1, Passes, Options)}
    end.

scan_toks([{'-',_Lh},{atom,_Ld,define}=Define|Toks], Cont, St, Passes, Options) ->
    scan_define(Toks, Define, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Ld,undef}=Undef|Toks], Cont, St, Passes, Options) ->
    scan_undef(Toks, Undef, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Li,include}=Inc|Toks], Cont, St, Passes, Options) ->
    scan_include(Toks, Inc, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Li,include_lib}=IncLib|Toks], Cont, St, Passes, Options) ->
    scan_include_lib(Toks, IncLib, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Li,ifdef}=IfDef|Toks], Cont, St, Passes, Options) ->
    scan_ifdef(Toks, IfDef, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Li,ifndef}=IfnDef|Toks], Cont, St, Passes, Options) ->
    scan_ifndef(Toks, IfnDef, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Le,'else'}=Else|Toks], Cont, St, Passes, Options) ->
    scan_else(Toks, Else, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{'if',_Le}=If|Toks], Cont, St, Passes, Options) ->
    scan_if(Toks, If, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Le,elif}=Elif|Toks], Cont, St, Passes, Options) ->
    scan_elif(Toks, Elif, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Le,endif}=Endif|Toks], Cont, St, Passes, Options) ->
    scan_endif(Toks, Endif, Cont, St, Passes, Options);
scan_toks([{'-',_Lh},{atom,_Lf,file}=FileToken|Toks0], Cont, St, Passes, Options) ->
    #{macs:=Macs, uses:=Uses} = St,
    case catch expand_macros(Toks0, {Macs, Uses}) of
        Toks1 when is_list(Toks1) ->
            scan_file(Toks1, FileToken, Cont, St, Passes, Options);
        {error,ErrL,What} ->
            {{error,{ErrL,epp,What}}, St, wait_req_scan(Cont, Passes, Options)}
    end;
scan_toks(Toks0, Cont, St, Passes, Options) ->
    #{macs:=Macs, uses:=Uses} = St,
    case catch expand_macros(Toks0, {Macs, Uses}) of
        Toks1 when is_list(Toks1) ->
            {{ok, Toks1}, St#{macs:=scan_module(Toks1, Macs)}, wait_req_scan(Cont, Passes, Options)};
        {error,ErrL,What} ->
            {{error,{ErrL,epp,What}}, St, wait_req_scan(Cont, Passes, Options)}
    end.

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
    scan_module_1(Ts, [], Ms);
scan_module([{'-',_Lh},{atom,_Lm,extends},{'(',_Ll}|Ts], Ms) ->
    scan_extends(Ts, [], Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',L}|Ts], As, Ms) ->
    %% Parameterized modules.
    scan_module_1([A,{')',L}|Ts], As, Ms);
scan_module_1([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'MODULE'},
                     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_module_1([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, [".",A|As], Ms);
scan_module_1([{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, As, Ms);
scan_module_1(_Ts, _As, Ms) -> Ms.

scan_extends([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'BASE_MODULE'},
                     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'BASE_MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_extends([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, [".",A|As], Ms);
scan_extends([{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, As, Ms);
scan_extends(_Ts, _As, Ms) -> Ms.

%% scan_define(Tokens, DefineToken, From, EppState)

scan_define([{'(',_Lp},{Type,_Lm,M}=Mac,{',',Lc}|Toks], _Def, Cont, St, Passes, Options)
  when Type =:= atom; Type =:= var ->
    #{macs:=Macs} = St,
    case catch macro_expansion(Toks, Lc) of
        Expansion when is_list(Expansion) ->
            case dict:find({atom,M}, Macs) of
                {ok, Defs} when is_list(Defs) ->
                    %% User defined macros: can be overloaded
                    case proplists:is_defined(none, Defs) of
                        true ->
                            {{error,{loc(Mac),epp,{redefine,M}}}, St, wait_req_scan(Cont, Passes, Options)};
                        false ->
                            scan_define_cont(Cont, St, Passes, Options,
                                             {atom, M},
                                             {none, {none,Expansion}})
                    end;
                {ok, _PreDef} ->
                    %% Predefined macros: cannot be overloaded
                    {{error,{loc(Mac),epp,{redefine_predef,M}}}, St, wait_req_scan(Cont, Passes, Options)};
                error ->
                    scan_define_cont(Cont, St, Passes, Options,
                                     {atom, M},
                                     {none, {none,Expansion}})
            end;
        {error,ErrL,What} ->
            {{error,{ErrL,epp,What}}, St, wait_req_scan(Cont, Passes, Options)}
    end;
scan_define([{'(',_Lp},{Type,_Lm,M}=Mac,{'(',_Lc}|Toks], Def, Cont, St, Passes, Options)
  when Type =:= atom; Type =:= var ->
    #{macs:=Macs} = St,
    case catch macro_pars(Toks, []) of
        {ok, {As,Me}} ->
            Len = length(As),
            case dict:find({atom,M}, Macs) of
                {ok, Defs} when is_list(Defs) ->
                    %% User defined macros: can be overloaded
                    case proplists:is_defined(Len, Defs) of
                        true ->
                            {{error,{loc(Mac),epp,{redefine,M}}}, St, wait_req_scan(Cont, Passes, Options)};
                        false ->
                            scan_define_cont(Cont, St, Passes, Options, {atom, M},
                                             {Len, {As, Me}})
                    end;
                {ok, _PreDef} ->
                    %% Predefined macros: cannot be overloaded
                    %% (There are currently no predefined F(...) macros.)
                    {{error,{loc(Mac),epp,{redefine_predef,M}}}, St, wait_req_scan(Cont, Passes, Options)};
                error ->
                    scan_define_cont(Cont, St, Passes, Options, {atom, M}, {Len, {As, Me}})
            end;
	{error,ErrL,What} ->
            {{error,{ErrL,epp,What}}, St, wait_req_scan(Cont, Passes, Options)};
        _ ->
            {{error,{loc(Def),epp,{bad,define}}}, St, wait_req_scan(Cont, Passes, Options)}
    end;
scan_define(_Toks, Def, Cont, St, Passes, Options) ->
    {{error,{loc(Def),epp,{bad,define}}}, St, wait_req_scan(Cont, Passes, Options)}.

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(Cont, St, Passes, Options, M, {Arity, Def}) ->
    #{macs:=Macs, uses:=Uses} = St,
    Ms = dict:append_list(M, [{Arity, Def}], Macs),
    U = dict:append_list(M, [{Arity, macro_uses(Def)}], Uses),
    scan_toks(Cont, St#{uses:=U, macs:=Ms}, Passes, Options).

macro_uses({_Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {'?', _} | Rest]) ->
    macro_ref(Rest);
macro_ref([{'?', _}, {atom, Lm, A} | Rest]) ->
    Arity = count_args(Rest, Lm, A),
    [{{atom, A}, Arity} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, Lm, A} | Rest]) ->
    Arity = count_args(Rest, Lm, A),
    [{{atom, A}, Arity} | macro_ref(Rest)];
macro_ref([_Token | Rest]) ->
    macro_ref(Rest).

%% scan_undef(Tokens, UndefToken, From, EppState)

scan_undef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, Cont, St, Passes, Options) ->
    #{macs:=Macs, uses:=Uses} = St,
    Macs1 = dict:erase({atom,M}, Macs),
    Uses1 = dict:erase({atom,M}, Uses),
    scan_toks(Cont, St#{macs:=Macs1, uses:=Uses1}, Passes, Options);
scan_undef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, Cont, St, Passes, Options) ->
    #{macs:=Macs, uses:=Uses} = St,
    Macs1 = dict:erase({atom,M}, Macs),
    Uses1 = dict:erase({atom,M}, Uses),
    scan_toks(Cont, St#{macs:=Macs1, uses:=Uses1}, Passes, Options);
scan_undef(_Toks, Undef, Cont, St, Passes, Options) ->
    {{error,{loc(Undef),epp,{bad,undef}}}, St, wait_req_scan(Cont, Passes, Options)}.

%% scan_include(Tokens, IncludeToken, From, St)

scan_include([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}], Inc,
	     Cont, St, Passes, Options) ->
    NewName = expand_var(NewName0),
    enter_file(NewName, Inc, Cont, St, Passes, Options);
scan_include(_Toks, Inc, Cont, St, Passes, Options) ->
    {{error,{abs_loc(Inc),epp,{bad,include}}}, St, wait_req_scan(Cont, Passes, Options)}.

%% scan_include_lib(Tokens, IncludeToken, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

find_lib_dir(NewName) ->
    [Lib | Rest] = filename:split(NewName),
    {code:lib_dir(list_to_atom(Lib)), Rest}.

scan_include_lib([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}],
                 Inc, Cont, St, Passes, Options) ->
    NewName = expand_var(NewName0),
    #{name:=FileName, location:=Location} = St,
    Loc = start_loc(Location),
    Path = [filename:dirname(FileName)
            |proplists:get_value(includes, Options, [])],

    case path_open(Path, NewName, Passes, Options) of
	{ok,St1,Cont1} ->
	    enter_file2(St1, Cont1, Cont, St, Passes, Options, Loc);
	{error,_E1} ->
	    case catch find_lib_dir(NewName) of
		{LibDir, Rest} when is_list(LibDir) ->
		    LibName = fname_join([LibDir | Rest]),
                    case ext_epp:open_file(LibName, Passes, Options) of
                        {ok, St1, Cont1} ->
			    enter_file2(St1, Cont1, Cont, St, Passes, Options, Loc);
			{error,_E2} ->
                            {{error,{abs_loc(Inc),epp, {include,lib,NewName}}}, St, wait_req_scan(Cont, Passes, Options)}
		    end;
		_Error ->
                    {{error,{abs_loc(Inc),epp,{include,lib,NewName}}},St, wait_req_scan(Cont, Passes, Options)}
	    end
    end;
scan_include_lib(_Toks, Inc, Cont, St, Passes, Options) ->
    {{error,{abs_loc(Inc),epp,{bad,include_lib}}}, St, wait_req_scan(Cont, Passes, Options)}.

%% scan_ifdef(Tokens, IfdefToken, From, EppState)
%% scan_ifndef(Tokens, IfdefToken, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, Cont, St, Passes, Options) ->
    #{macs:=Macs, istk:=IStk} = St,
    case dict:find({atom,M}, Macs) of
	{ok,_Def} ->
            scan_toks(Cont, St#{istk:=[ifdef|IStk]}, Passes, Options);
	error ->
	    skip_toks(Cont, St, Passes, Options, [ifdef])
    end;
scan_ifdef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfD, Cont, St, Passes, Options) ->
    #{macs:=Macs, istk:=IStk} = St,
    case dict:find({atom,M}, Macs) of
	{ok,_Def} ->
            scan_toks(Cont, St#{istk:=[ifdef|IStk]}, Passes, Options);
	error ->
	    skip_toks(Cont, St, Passes, Options, [ifdef])
    end;
scan_ifdef(_Toks, IfDef, Cont, St, Passes, Options) ->
    {{error,{loc(IfDef),epp,{bad,ifdef}}}, St, wait_req_skip(Cont, Passes, Options, [ifdef])}.

scan_ifndef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, Cont, St, Passes, Options) ->
    #{macs:=Macs, istk:=IStk} = St,
    case dict:find({atom,M}, Macs) of
	{ok,_Def} ->
	    skip_toks(Cont, St, Passes, Options, [ifndef]);
	error ->
            scan_toks(Cont, St#{istk:=[ifndef|IStk]}, Passes, Options)
    end;
scan_ifndef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _IfnD, Cont, St, Passes, Options) ->
    #{macs:=Macs, istk:=IStk} = St,
    case dict:find({atom,M}, Macs) of
	{ok,_Def} ->
	    skip_toks(Cont, St, Passes, Options, [ifndef]);
	error ->
            scan_toks(Cont, St#{istk:=[ifndef|IStk]}, Passes, Options)
    end;
scan_ifndef(_Toks, IfnDef, Cont, St, Passes, Options) ->
    {{error,{loc(IfnDef),epp,{bad,ifndef}}},St, wait_req_skip(Cont, Passes, Options, [ifndef])}.

%% scan_else(Tokens, ElseToken, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,_Ld}], Else, Cont, St, Passes, Options) ->
    #{istk:=IStk} = St,
    case IStk of
	['else'|Cis] ->
	    {{error,{loc(Else), epp,{illegal,"repeated",'else'}}}, St#{istk:=Cis}, wait_req_skip(Cont, Passes, Options, ['else'])};
	[_I|Cis] ->
	    skip_toks(Cont, St#{istk:=Cis}, Passes, Options, ['else']);
	[] ->
	    {{error,{loc(Else),epp, {illegal,"unbalanced",'else'}}}, St, wait_req_scan(Cont, Passes, Options)}
    end;
scan_else(_Toks, Else, Cont, St, Passes, Options) ->
    {{error,{loc(Else),epp,{bad,'else'}}}, St, wait_req_scan(Cont, Passes, Options)}.

%% scan_if(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_if(_Toks, If, Cont, St, Passes, Options) ->
    {{error,{loc(If),epp,{'NYI','if'}}}, St, wait_req_skip(Cont, Passes, Options, ['if'])}.

%% scan_elif(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(_Toks, Elif, Cont, St, Passes, Options) ->
    {{error,{loc(Elif),epp,{'NYI','elif'}}}, St, wait_req_skip(Cont, Passes, Options, ['elif'])}.

%% scan_endif(Tokens, EndifToken, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,_Ld}], Endif, Cont, St, Passes, Options) ->
    #{istk:=IStk} = St,
    case IStk of
	[_I|Cis] ->
            scan_toks(Cont, St#{istk:=Cis}, Passes, Options);
	[] ->
	    {{error,{loc(Endif),epp,{illegal,"unbalanced",endif}}}, St, wait_req_scan(Cont, Passes, Options)}
    end;
scan_endif(_Toks, Endif, Cont, St, Passes, Options) ->
    {{error,{loc(Endif),epp,{bad,endif}}}, St, wait_req_scan(Cont, Passes, Options)}.

%% scan_file(Tokens, FileToken, From, EppState)
%%  Set the current file and line to the given file and line.
%%  Note that the line of the attribute itself is kept.

scan_file([{'(',_Llp},{string,_Ls,Name},{',',_Lc},{integer,_Li,Ln},{')',_Lrp},
           {dot,_Ld}], Tf, Cont, St, Passes, Options) ->
    #{macs:=Macs, delta:=Delta, location:=Location} = St,
    Rep = ext_epp:enter_file_reply(Name, Ln, neg_line(abs_loc(Tf))),
    Ms = dict:store({atom,'FILE'}, {none,[{string,1,Name}]}, Macs),
    Locf = loc(Tf),
    NewLoc = new_location(Ln, Location, Locf),
    Delta1 = abs(ext_epp:get_line(element(2, Tf)))-Ln + Delta,
    {Rep, St#{name2:=Name,location:=NewLoc,delta:=Delta1,macs:=Ms}, wait_req_scan(Cont, Passes, Options)};
scan_file(_Toks, Tf, Cont, St, Passes, Options) ->
    {{error,{loc(Tf),epp,{bad,file}}}, St, wait_req_scan(Cont, Passes, Options)}.

new_location(Ln, Le, Lf) when is_integer(Lf) ->
    Ln+(Le-Lf);
new_location(Ln, {Le,_}, {Lf,_}) ->
    {Ln+(Le-Lf),1}.

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(Cont, St, Passes, Options, [I|Sis]) ->
    case Cont(St) of
	{{ok,[{'-',_Lh},{atom,_Li,ifdef}|_Toks]},St1,Cont1} ->
	    skip_toks(Cont1, St1, Passes, Options, [ifdef,I|Sis]);
	{{ok,[{'-',_Lh},{atom,_Li,ifndef}|_Toks]},St1,Cont1} ->
	    skip_toks(Cont1, St1, Passes, Options, [ifndef,I|Sis]);
	{{ok,[{'-',_Lh},{'if',_Li}|_Toks]},St1,Cont1} ->
	    skip_toks(Cont1, St1, Passes, Options, ['if',I|Sis]);
	{{ok,[{'-',_Lh},{atom,_Le,'else'}=Else|_Toks]},St1,Cont1}->
	    skip_else(Else, Cont1, St1, Passes, Options, [I|Sis]);
	{{ok,[{'-',_Lh},{atom,_Le,endif}|_Toks]},St1,Cont1} ->
	    skip_toks(Cont1, St1, Passes, Options, Sis);
	{{ok,_Toks},St1,Cont1} ->
	    skip_toks(Cont1, St1, Passes, Options, [I|Sis]);
	{{error,E},St1,Cont1} ->
	    case E of
		{_,file_io_server,invalid_unicode} ->
		    %% The compiler needs to know that there was
		    %% invalid unicode characters in the file
		    %% (and there is no point in continuing anyway
		    %% since io server process has terminated).
                    {{error, E}, St1,
                     fun (State) ->
                             %% TODO: close file
                             leave_file(eof, State, Passes, Options)
                     end};
		_ ->
		    %% Some other invalid token, such as a bad floating
		    %% point number. Just ignore it.
		    skip_toks(Cont1, St1, Passes, Options, [I|Sis])
	    end;
	{{eof, _},St1, eof} ->
	    leave_file(eof, St1#{istk:=[I|Sis]}, Passes, Options)
    end;
skip_toks(Cont, St, Passes, Options, []) ->
    scan_toks(Cont, St, Passes, Options).

skip_else(Else, Cont, St, Passes, Options, ['else'|Sis]) ->
    {{error,{loc(Else),epp,{illegal,"repeated",'else'}}}, St, wait_req_skip(Cont, Passes, Options, ['else'|Sis])};
skip_else(_Else, Cont, St, Passes, Options, [_I]) ->
    #{istk:=IStk} = St,
    scan_toks(Cont, St#{istk:=['else'|IStk]}, Passes, Options);
skip_else(_Else, Cont, St, Passes, Options, Sis) ->
    skip_toks(Cont, St, Passes, Options, Sis).

%% macro_pars(Tokens, ArgStack)
%% macro_expansion(Tokens, Line)
%%  Extract the macro parameters and the expansion from a macro definition.

macro_pars([{')',_Lp}, {',',Ld}|Ex], Args) ->
    {ok, {lists:reverse(Args), macro_expansion(Ex, Ld)}};
macro_pars([{var,_,Name}, {')',_Lp}, {',',Ld}|Ex], Args) ->
    false = lists:member(Name, Args),		%Prolog is nice
    {ok, {lists:reverse([Name|Args]), macro_expansion(Ex, Ld)}};
macro_pars([{var,_L,Name}, {',',_}|Ts], Args) ->
    false = lists:member(Name, Args),
    macro_pars(Ts, [Name|Args]).

macro_expansion([{')',_Lp},{dot,_Ld}], _L0) -> [];
macro_expansion([{dot,Ld}], _L0) -> throw({error,Ld,missing_parenthesis});
macro_expansion([T|Ts], _L0) ->
    [T|macro_expansion(Ts, element(2, T))];
macro_expansion([], L0) -> throw({error,L0,premature_end}).

%% expand_macros(Tokens, Macros)
%% expand_macro(Tokens, MacroToken, RestTokens)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same location as the macro call.

expand_macros(Type, MacT, M, Toks, Ms0) ->
    %% (Type will always be 'atom')
    {Ms, U} = Ms0,
    Lm = loc(MacT),
    Tinfo = element(2, MacT),
    case expand_macro1(Type, Lm, M, Toks, Ms) of
	{ok,{none,Exp}} ->
	    check_uses([{{Type,M}, none}], [], U, Lm),
	    Toks1 = expand_macros(expand_macro(Exp, Tinfo, [], dict:new()), Ms0),
	    expand_macros(Toks1++Toks, Ms0);
	{ok,{As,Exp}} ->
	    check_uses([{{Type,M}, length(As)}], [], U, Lm),
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, dict:new()),
	    expand_macros(expand_macro(Exp, Tinfo, Toks1, Bs), Ms0)
    end.

expand_macro1(Type, Lm, M, Toks, Ms) ->
    Arity = count_args(Toks, Lm, M),
    case dict:find({Type,M}, Ms) of
        error -> %% macro not found
            throw({error,Lm,{undefined,M,Arity}});
        {ok, undefined} -> %% Predefined macro without definition
            throw({error,Lm,{undefined,M,Arity}});
        {ok, [{none, Def}]} ->
            {ok, Def};
        {ok, Defs} when is_list(Defs) ->
            case proplists:get_value(Arity, Defs) of
                undefined ->
                    throw({error,Lm,{mismatch,M}});
                Def ->
                    {ok, Def}
            end;
        {ok, PreDef} -> %% Predefined macro
            {ok, PreDef}
    end.

check_uses([], _Anc, _U, _Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {{_, Name},Arity} = M,
	    throw({error,Lm,{circular,Name,Arity}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.

get_macro_uses({M,Arity}, U) ->
    case dict:find(M, U) of
	error ->
	    [];
	{ok, L} ->
	    proplists:get_value(Arity, L, proplists:get_value(none, L, []))
    end.

%% Macro expansion
%% Note: io:scan_erl_form() does not return comments or white spaces.
expand_macros([{'?',_Lq},{atom,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%% Special macros
expand_macros([{'?',_Lq},{var,Lm,'LINE'}=Tok|Toks], Ms) ->
    {line,Line} = erl_scan:token_info(Tok, line),
    [{integer,Lm,Line}|expand_macros(Toks, Ms)];
expand_macros([{'?',_Lq},{var,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%% Illegal macros
expand_macros([{'?',_Lq},Token|_Toks], _Ms) ->
    T = case erl_scan:token_info(Token, text) of
            {text,Text} ->
                Text;
            undefined ->
                {symbol,Symbol} = erl_scan:token_info(Token, symbol),
                io_lib:write(Symbol)
        end,
    throw({error,loc(Token),{call,[$?|T]}});
expand_macros([T|Ts], Ms) ->
    [T|expand_macros(Ts, Ms)];
expand_macros([], _Ms) -> [].

%% bind_args(Tokens, MacroLocation, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call.

bind_args([{'(',_Llp},{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',_Llp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
bind_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

macro_args([{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',_Lc}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
macro_args([], Lm, M, _As, _Bs) ->
    throw({error,Lm,{arg_error,M}}); % Cannot happen.
macro_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

store_arg(L, M, _A, [], _Bs) ->
    throw({error,L,{mismatch,M}});
store_arg(_L, _M, A, Arg, Bs) ->
    dict:store(A, Arg, Bs).

%% count_args(Tokens, MacroLine, MacroName)
%%  Count the number of arguments in a macro call.
count_args([{'(', _Llp},{')',_Lrp}|_Toks], _Lm, _M) ->
    0;
count_args([{'(', _Llp},{',',_Lc}|_Toks], Lm, M) ->
    throw({error,Lm,{arg_error,M}});
count_args([{'(',_Llp}|Toks0], Lm, M) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, 1);
count_args(_Toks, _Lm, _M) ->
    none.

count_args([{')',_Lrp}|_Toks], _Lm, _M, NbArgs) ->
    NbArgs;
count_args([{',',_Lc},{')',_Lrp}|_Toks], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args([{',',_Lc}|Toks0], Lm, M, NbArgs) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, NbArgs+1);
count_args([], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args(_Toks, Lm, M, _NbArgs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".

macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'fun',Lc}|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'fun',Lc}|Arg]);
macro_arg([{'fun',_}=Fun,{var,_,_}=Name|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [Name,Fun|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{'try',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'try',Lr}|Arg]);
macro_arg([{'cond',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'cond',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], _E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroTokenInfo, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, MacroLocation, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the location of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
	    expand_arg(Val, Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
            expand_arg(stringify(Val, L), Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([T|Ts], L, Rest, Bs) ->
    [setelement(2, T, L)|expand_macro(Ts, L, Rest, Bs)];
expand_macro([], _L, Rest, _Bs) -> Rest.

expand_arg([A|As], Ts, _L, Rest, Bs) ->
    %% It is not obvious that the location of arguments should replace L.
    NextL = element(2, A),
    [A|expand_arg(As, Ts, NextL, Rest, Bs)];
expand_arg([], Ts, L, Rest, Bs) ->
    expand_macro(Ts, L, Rest, Bs).

%%% stringify(Ts, L) returns a list of one token: a string which when
%%% tokenized would yield the token list Ts.

%% erl_scan:token_info(T, text) is not backward compatible with this.
%% Note that escaped characters will be replaced by themselves.
token_src({dot, _}) ->
    ".";
token_src({X, _}) when is_atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({char,_,C}) ->
    io_lib:write_char(C);
token_src({string, _, X}) ->
    io_lib:write_string(X);
token_src({_, _, X}) ->
    io_lib:format("~w", [X]).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~ts", [token_src(T)]) | stringify1(Tokens)].

stringify(Ts, L) ->
    [$\s | S] = lists:flatten(stringify1(Ts)),
    [{string, L, S}].

expand_var([$$ | _] = NewName) ->
    case catch expand_var1(NewName) of
	{ok, ExpName} ->
	    ExpName;
	_ ->
	    NewName
    end;
expand_var(NewName) ->
    NewName.

expand_var1(NewName) ->
    [[$$ | Var] | Rest] = filename:split(NewName),
    Value = os:getenv(Var),
    true = Value =/= false,
    {ok, fname_join([Value | Rest])}.

fname_join(["." | [_|_]=Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).

loc(Token) ->
    {location,Location} = erl_scan:token_info(Token, location),
    Location.

abs_loc(Token) ->
    loc(setelement(2, Token, abs_line(element(2, Token)))).

neg_line(L) ->
    erl_scan:set_attribute(line, L, fun(Line) -> -abs(Line) end).

abs_line(L) ->
    erl_scan:set_attribute(line, L, fun(Line) -> abs(Line) end).

add_line(L, Offset) ->
    erl_scan:set_attribute(line, L, fun(Line) -> Line+Offset end).

start_loc(Line) when is_integer(Line) ->
    1;
start_loc({_Line, _Column}) ->
    {1,1}.
