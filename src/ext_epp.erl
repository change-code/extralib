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

-module(ext_epp).

-export([parse_file/2]).

-export([open_file/3, enter_file_reply/3, get_line/1]).


open_file(FileName, [], Options) ->
    case file:open(FileName, [read]) of
        {ok, File} ->
            Encoding = proplists:get_value(default_encoding, Options, epp:default_encoding()),
            Encoding1 = epp:set_encoding(File, Encoding),
            Opts = proplists:get_value(scan_options, Options, []),
            State =
                #{file => File,
                  encoding => Encoding1,
                  name => FileName,
                  location => 1},
            Rep = enter_file_reply(FileName, 1, 1),
            {ok, State,
             fun (St) ->
                     {Rep, St, scan_form(Opts)}
             end};
        {error, _} = Error ->
            Error
    end;
open_file(FileName, [Module|Passes], Options) ->
    Module:open_file(FileName, Passes, Options).


eof_form(State = #{file := File, location := Location}) ->
    file:close(File),
    {{eof, Location}, State, eof}.


scan_form(Opts) ->
    fun Scan(State = #{file := File, location := Location}) ->
            case io:scan_erl_form(File, '', Location, Opts) of
                {ok, Tokens, EndLocation} ->
                    {{ok, Tokens}, State#{location => EndLocation}, Scan};
                {error, Error, EndLocation} ->
                    {{error, Error}, State#{location => EndLocation}, Scan};
                {eof, EndLocation} ->
                    file:close(File),
                    {{eof, EndLocation}, State#{location => EndLocation}, eof};
                {error, _} ->
                    {{error, {Location, epp, cannot_parse}}, State, fun eof_form/1}
            end
    end.


parse_file(FileName, Options) ->
    Passes = proplists:get_value(passes, Options, []),
    case open_file(FileName, Passes, Options) of
        {ok, State, Cont} ->
            Forms = parse_forms(State, Cont),
            {ok, Forms};
        {error, _} = Error ->
            Error
    end.


parse_forms(State, Cont) ->
    case Cont(State) of
        {{ok, Tokens}, State1, Cont1} ->
            case erl_parse:parse_form(Tokens) of
                {ok, {attribute,La,record,{Record, Fields}} = Form} ->
                    case epp:normalize_typed_record_fields(Fields) of
                        {typed, NewFields} ->
                            [{attribute, La, record, {Record, NewFields}},
                             {attribute, La, type,
                              {{record, Record}, Fields, []}}
                             |parse_forms(State1, Cont1)];
                        not_typed ->
                            [Form|parse_forms(State1, Cont1)]
                    end;
                {ok, Form} ->
                    [Form|parse_forms(State1, Cont1)];
                {error, _} = Error ->
                    [Error|parse_forms(State1, Cont1)]
            end;
        {{error, _} = Error, State1, Cont1} ->
            [Error|parse_forms(State1, Cont1)];
        {{eof, _} = EOF, _, eof} ->
            [EOF]
    end.


enter_file_reply(Name, Location, AtLocation) ->
    Attr = loc_attr(AtLocation),
    Rep = {ok, [{'-',Attr},{atom,Attr,file},{'(',Attr},
		{string,Attr,file_name(Name)},{',',Attr},
		{integer,Attr,get_line(Location)},{')',Location},
                {dot,Attr}]},
    Rep.


%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when is_atom(N) ->
    atom_to_list(N).

%% The line only. (Other tokens may have the column and text as well...)
loc_attr(Line) when is_integer(Line) ->
    Line;
loc_attr({Line,_Column}) ->
    Line.

get_line(Line) when is_integer(Line) ->
    Line;
get_line({Line,_Column}) ->
    Line.
