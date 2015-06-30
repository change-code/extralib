%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%
-module(ext_gen_server).

%% API
-export([start/3, start/4,
         start_link/3, start_link/4,
         enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% System exports
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         system_get_state/1,
         system_replace_state/2,
         format_status/2]).

%% Internal exports
-export([init_it/6, code_change/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback system_code_change(State :: term(), Module :: module(),
                             OldVsn :: (term() | {down, term()}), Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks([format_status/2]).

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%-----------------------------------------------------------------
%% enter_loop(Mod, Options, State, <ServerName>, <TimeOut>) ->_
%%
%% Description: Makes an existing process into a gen_server.
%%              The calling process will enter the gen_server receive
%%              loop and become a gen_server process.
%%              The process *must* have been started using one of the
%%              start functions in proc_lib, see proc_lib(3).
%%              The user is responsible for any initialization of the
%%              process, including registering a name for it.
%%-----------------------------------------------------------------
enter_loop(Mod, Options, State) ->
    enter_loop(Mod, Options, State, self(), infinity).

enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when Scope == local; Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, ServerName = {via, _, _}) ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, Timeout) ->
    enter_loop(Mod, Options, State, self(), Timeout).

enter_loop(Mod, Options, State, ServerName, Timeout) ->
    Name = get_proc_name(ServerName),
    Parent = get_parent(),
    Debug = debug_options(Name, Options),
    loop(Parent, Name, State, Mod, Timeout, Debug).

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = debug_options(Name, Options),
    case catch Mod:init(Args) of
        {ok, State} ->
            proc_lib:init_ack(Starter, {ok, self()}),
            loop(Parent, Name, State, Mod, infinity, Debug);
        {ok, State, Timeout} ->
            proc_lib:init_ack(Starter, {ok, self()}),
            loop(Parent, Name, State, Mod, Timeout, Debug);
        {stop, Reason} ->
            %% For consistency, we must make sure that the
            %% registered name (if any) is unregistered before
            %% the parent process is notified about the failure.
            %% (Otherwise, the parent process could get
            %% an 'already_started' error if it immediately
            %% tried starting the process again.)
            unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        ignore ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, ignore),
            exit(normal);
        {'EXIT', Reason} ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        Else ->
            Error = {bad_return_value, Else},
            proc_lib:init_ack(Starter, {error, Error}),
            exit(Error)
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    _ = (catch unregister(Name));
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name);
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
loop(Parent, Name, State, Mod, hibernate, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, Debug]);
loop(Parent, Name, State, Mod, Time, Debug) ->
    Msg = receive
              Input ->
                  Input
          after Time ->
                  timeout
          end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).

wake_hib(Parent, Name, State, Mod, Debug) ->
    Msg = receive
              Input ->
                  Input
          end,
    decode_msg(Msg, Parent, Name, State, Mod, hibernate, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                                  [Name, State, Mod, Time], Hib);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, Name, Msg, Mod, State, Debug);
        _Msg when Debug =:= [] ->
            handle_msg(Msg, Parent, Name, State, Mod);
        _Msg ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3,
                                      Name, {in, Msg}),
            handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.


%% ---------------------------------------------------
%% Helper functions for try-catch of callbacks.
%% Returns the return value of the callback, or
%% {'EXIT', ExitReason, ReportReason} (if an exception occurs)
%%
%% ExitReason is the reason that shall be used when the process
%% terminates.
%%
%% ReportReason is the reason that shall be printed in the error
%% report.
%%
%% These functions are introduced in order to add the stack trace in
%% the error report produced when a callback is terminated with
%% erlang:exit/1 (OTP-12263).
%% ---------------------------------------------------

try_dispatch({'$gen_cast', Msg}, Mod, State) ->
    try_dispatch(Mod, handle_cast, Msg, State);
try_dispatch(Info, Mod, State) ->
    try_dispatch(Mod, handle_info, Info, State).

try_dispatch(Mod, Func, Msg, State) ->
    try
        {ok, Mod:Func(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        error:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
        exit:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', R, {R, Stacktrace}}
    end.

try_handle_call(Mod, Msg, From, State) ->
    try
        {ok, Mod:handle_call(Msg, From, State)}
    catch
        throw:R ->
            {ok, R};
        error:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
        exit:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', R, {R, Stacktrace}}
    end.

try_terminate(Mod, Reason, State) ->
    try
        {ok, Mod:terminate(Reason, State)}
    catch
        throw:R ->
            {ok, R};
        error:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
        exit:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', R, {R, Stacktrace}}
    end.


%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
        {ok, {reply, Reply, NState}} ->
            reply(From, Reply),
            loop(Parent, Name, NState, Mod, infinity, []);
        {ok, {reply, Reply, NState, Time1}} ->
            reply(From, Reply),
            loop(Parent, Name, NState, Mod, Time1, []);
        {ok, {noreply, NState}} ->
            loop(Parent, Name, NState, Mod, infinity, []);
        {ok, {noreply, NState, Time1}} ->
            loop(Parent, Name, NState, Mod, Time1, []);
        {ok, {stop, Reason, Reply, NState}} ->
            {'EXIT', R} =
                (catch terminate(Reason, Name, Msg, Mod, NState, [])),
            reply(From, Reply),
            exit(R);
        Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State)
    end;
handle_msg(Msg, Parent, Name, State, Mod) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
        {ok, {reply, Reply, NState}} ->
            Debug1 = reply(Name, From, Reply, NState, Debug),
            loop(Parent, Name, NState, Mod, infinity, Debug1);
        {ok, {reply, Reply, NState, Time1}} ->
            Debug1 = reply(Name, From, Reply, NState, Debug),
            loop(Parent, Name, NState, Mod, Time1, Debug1);
        {ok, {noreply, NState}} ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                                      {noreply, NState}),
            loop(Parent, Name, NState, Mod, infinity, Debug1);
        {ok, {noreply, NState, Time1}} ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                                      {noreply, NState}),
            loop(Parent, Name, NState, Mod, Time1, Debug1);
        {ok, {stop, Reason, Reply, NState}} ->
            {'EXIT', R} =
                (catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
            _ = reply(Name, From, Reply, NState, Debug),
            exit(R);
        Other ->
            handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
    end;
handle_msg(Msg, Parent, Name, State, Mod, Debug) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug).

handle_common_reply(Reply, Parent, Name, Msg, Mod, State) ->
    case Reply of
        {ok, {noreply, NState}} ->
            loop(Parent, Name, NState, Mod, infinity, []);
        {ok, {noreply, NState, Time1}} ->
            loop(Parent, Name, NState, Mod, Time1, []);
        {ok, {stop, Reason, NState}} ->
            terminate(Reason, Name, Msg, Mod, NState, []);
        {'EXIT', ExitReason, ReportReason} ->
            terminate(ExitReason, ReportReason, Name, Msg, Mod, State, []);
        {ok, BadReply} ->
            terminate({bad_return_value, BadReply}, Name, Msg, Mod, State, [])
    end.

handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug) ->
    case Reply of
        {ok, {noreply, NState}} ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                                      {noreply, NState}),
            loop(Parent, Name, NState, Mod, infinity, Debug1);
        {ok, {noreply, NState, Time1}} ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                                      {noreply, NState}),
            loop(Parent, Name, NState, Mod, Time1, Debug1);
        {ok, {stop, Reason, NState}} ->
            terminate(Reason, Name, Msg, Mod, NState, Debug);
        {'EXIT', ExitReason, ReportReason} ->
            terminate(ExitReason, ReportReason, Name, Msg, Mod, State, Debug);
        {ok, BadReply} ->
            terminate({bad_return_value, BadReply}, Name, Msg, Mod, State, Debug)
    end.

reply(Name, {To, Tag}, Reply, State, Debug) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
                     {out, Reply, To, State} ).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, State, Mod, Time]) ->
    loop(Parent, Name, State, Mod, Time, Debug).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time]) ->
    terminate(Reason, Name, [], Mod, State, Debug).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

system_code_change(State, ?MODULE, OldVsn, Extra) ->
    case catch ?MODULE:code_change(OldVsn, State, Extra) of
        {ok, NewState} -> {ok, NewState};
        Else -> Else
    end;
system_code_change([Name, State, Mod, Time], Module, OldVsn, Extra) ->
    case catch Mod:system_code_change(State, Module, OldVsn, Extra) of
        {ok, NewState} -> {ok, [Name, NewState, Mod, Time]};
        Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time]) ->
    {ok, State}.

system_replace_state(StateFun, [Name, State, Mod, Time]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, Mod, Time]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
        {'$gen_call', {From, _Tag}, Call} ->
            io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
                      [Name, Call, From]);
        {'$gen_cast', Cast} ->
            io:format(Dev, "*DBG* ~p got cast ~p~n",
                      [Name, Cast]);
        _ ->
            io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n",
              [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

-spec terminate(_, _, _, _, _, _) -> no_return().
terminate(Reason, Name, Msg, Mod, State, Debug) ->
    terminate(Reason, Reason, Name, Msg, Mod, State, Debug).

-spec terminate(_, _, _, _, _, _, _) -> no_return().
terminate(ExitReason, ReportReason, Name, Msg, Mod, State, Debug) ->
    Reply = try_terminate(Mod, ExitReason, State),
    case Reply of
        {'EXIT', ExitReason1, ReportReason1} ->
            FmtState = format_status(terminate, Mod, get(), State),
            error_info(ReportReason1, Name, Msg, FmtState, Debug),
            exit(ExitReason1);
        _ ->
            case ExitReason of
                normal ->
                    exit(normal);
                shutdown ->
                    exit(shutdown);
                {shutdown,_}=Shutdown ->
                    exit(Shutdown);
                _ ->
                    FmtState = format_status(terminate, Mod, get(), State),
                    error_info(ReportReason, Name, Msg, FmtState, Debug),
                    exit(ExitReason)
            end
    end.

error_info(_Reason, application_controller, _Msg, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Reason, Name, Msg, State, Debug) ->
    Reason1 =
        case Reason of
            {undef,[{M,F,A,L}|MFAs]} ->
                case code:is_loaded(M) of
                    false ->
                        {'module could not be loaded',[{M,F,A,L}|MFAs]};
                    _ ->
                        case erlang:function_exported(M, F, length(A)) of
                            true ->
                                Reason;
                            false ->
                                {'function not exported',[{M,F,A,L}|MFAs]}
                        end
                end;
            _ ->
                Reason
        end,
    error_logger:format("** Generic server ~p terminating \n"
                        "** Last message in was ~p~n"
                        "** When Server state == ~p~n"
                        "** Reason for termination == ~n** ~p~n",
                        [Name, Msg, State, Reason1]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Name, Opts) ->
    case opt(debug, Opts) of
        {ok, Options} -> dbg_opts(Name, Options);
        _ -> []
    end.

dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
        {'EXIT',_} ->
            error_logger:format("~p: ignoring erroneous debug options - ~p~n",
                                [Name, Opts]),
            [];
        Dbg ->
            Dbg
    end.

get_proc_name(Pid) when is_pid(Pid) ->
    Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
        {registered_name, Name} ->
            Name;
        {registered_name, _Name} ->
            exit(process_not_registered);
        [] ->
            exit(process_not_registered)
    end;
get_proc_name({global, Name}) ->
    case global:whereis_name(Name) of
        undefined ->
            exit(process_not_registered_globally);
        Pid when Pid =:= self() ->
            Name;
        _Pid ->
            exit(process_not_registered_globally)
    end;
get_proc_name({via, Mod, Name}) ->
    case Mod:whereis_name(Name) of
        undefined ->
            exit({process_not_registered_via, Mod});
        Pid when Pid =:= self() ->
            Name;
        _Pid ->
            exit({process_not_registered_via, Mod})
    end.

get_parent() ->
    case get('$ancestors') of
        [Parent | _] when is_pid(Parent)->
            Parent;
        [Parent | _] when is_atom(Parent)->
            name_to_pid(Parent);
        _ ->
            exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
        undefined ->
            case global:whereis_name(Name) of
                undefined ->
                    exit(could_not_find_registered_name);
                Pid ->
                    Pid
            end;
        Pid ->
            Pid
    end.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time]] = StatusData,
    Header = gen:format_status_header("Status for generic server", Name),
    Log = sys:get_debug(log, Debug, []),
    Specfic = case format_status(Opt, Mod, PDict, State) of
                  S when is_list(S) -> S;
                  S -> [S]
              end,
    [{header, Header},
     {data, [{"Status", SysState},
             {"Parent", Parent},
             {"Logged events", Log}]} |
     Specfic].

format_status(Opt, Mod, PDict, State) ->
    DefStatus = case Opt of
                    terminate -> State;
                    _ -> [{data, [{"State", State}]}]
                end,
    case erlang:function_exported(Mod, format_status, 2) of
        true ->
            case catch Mod:format_status(Opt, [PDict, State]) of
                {'EXIT', _} -> DefStatus;
                Else -> Else
            end;
        _ ->
            DefStatus
    end.
