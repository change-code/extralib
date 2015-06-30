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

-module(ext_gen_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).


all() ->
    [start, crash, call, cast, info, hibernate,
     call_format_status, error_format_status, terminate_crash_format,
     get_state, replace_state].


start(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    %% anonymous
    ?line {ok, Pid0} = ext_gen_server:start(?MODULE, [], []),
    ?line ok = gen_server:call(Pid0, started_p),
    ?line ok = gen_server:call(Pid0, stop),
    ?line busy_wait_for_process(Pid0,600),
    ?line {'EXIT', {noproc,_}} = (catch gen_server:call(Pid0, started_p, 1)),

    %% anonymous with timeout
    ?line {ok, Pid00} = ext_gen_server:start(?MODULE, [],
                                             [{timeout,1000}]),
    ?line ok = gen_server:call(Pid00, started_p),
    ?line ok = gen_server:call(Pid00, stop),
    ?line {error, timeout} = gen_server:start(?MODULE, sleep,
                                              [{timeout,100}]),

    %% anonymous with ignore
    ?line ignore = ext_gen_server:start(?MODULE, ignore, []),

    %% anonymous with stop
    ?line {error, stopped} = ext_gen_server:start(?MODULE, stop, []),

    %% anonymous linked
    ?line {ok, Pid1} =
        ext_gen_server:start_link(?MODULE, [], []),
    ?line ok = gen_server:call(Pid1, started_p),
    ?line ok = gen_server:call(Pid1, stop),
    ?line receive
              {'EXIT', Pid1, stopped} ->
                  ok
          after 5000 ->
                  test_server:fail(not_stopped)
          end,

    %% local register
    ?line {ok, Pid2} =
        ext_gen_server:start({local, my_test_name},
                             ?MODULE, [], []),
    ?line ok = gen_server:call(my_test_name, started_p),
    ?line {error, {already_started, Pid2}} =
        ext_gen_server:start({local, my_test_name},
                             ?MODULE, [], []),
    ?line ok = gen_server:call(my_test_name, stop),

    ?line busy_wait_for_process(Pid2,600),

    ?line {'EXIT', {noproc,_}} = (catch gen_server:call(Pid2, started_p, 10)),

    %% local register linked
    ?line {ok, Pid3} =
        ext_gen_server:start_link({local, my_test_name},
                                  ?MODULE, [], []),
    ?line ok = gen_server:call(my_test_name, started_p),
    ?line {error, {already_started, Pid3}} =
        gen_server:start({local, my_test_name},
                         ?MODULE, [], []),
    ?line ok = gen_server:call(my_test_name, stop),
    ?line receive
              {'EXIT', Pid3, stopped} ->
                  ok
          after 5000 ->
                  test_server:fail(not_stopped)
          end,

    process_flag(trap_exit, OldFl),
    ok.


crash(Config) when is_list(Config) ->
    ?line error_logger_forwarder:register(),

    process_flag(trap_exit, true),

    %% This crash should not generate a crash report.
    ?line {ok,Pid0} = gen_server:start_link(?MODULE, [], []),
    ?line {'EXIT',{{shutdown,reason},_}} =
        (catch gen_server:call(Pid0, shutdown_reason)),
    receive {'EXIT',Pid0,{shutdown,reason}} -> ok end,

    %% This crash should not generate a crash report.
    ?line {ok,Pid1} = gen_server:start_link(?MODULE, {state,state1}, []),
    ?line {'EXIT',{{shutdown,stop_reason},_}} =
        (catch gen_server:call(Pid1, stop_shutdown_reason)),
    receive {'EXIT',Pid1,{shutdown,stop_reason}} -> ok end,

    %% This crash should not generate a crash report.
    ?line {ok,Pid2} = gen_server:start_link(?MODULE, [], []),
    ?line {'EXIT',{shutdown,_}} =
        (catch gen_server:call(Pid2, exit_shutdown)),
    receive {'EXIT',Pid2,shutdown} -> ok end,

    %% This crash should not generate a crash report.
    ?line {ok,Pid3} = gen_server:start_link(?MODULE, {state,state3}, []),
    ?line {'EXIT',{shutdown,_}} =
        (catch gen_server:call(Pid3, stop_shutdown)),
    receive {'EXIT',Pid3,shutdown} -> ok end,

    process_flag(trap_exit, false),

    %% This crash should generate a crash report and a report
    %% from gen_server.
    ?line {ok,Pid4} = gen_server:start(?MODULE, {state,state4}, []),
    ?line {'EXIT',{crashed,_}} = (catch gen_server:call(Pid4, crash)),
    receive
        {error,_GroupLeader4,{Pid4,
                              "** Generic server"++_,
                              [Pid4,crash,{formatted, state4},
                               {crashed,[{?MODULE,handle_call,3,_}
                                         |_Stacktrace]}]}} ->
            ok;
        Other4a ->
            ?line io:format("Unexpected: ~p", [Other4a]),
            ?line ?t:fail()
    end,
    receive
        {error_report,_,{Pid4,crash_report,[List4|_]}} ->
            {exit,crashed,_} = proplists:get_value(error_info, List4),
            Pid4 = proplists:get_value(pid, List4);
        Other4 ->
            ?line io:format("Unexpected: ~p", [Other4]),
            ?line ?t:fail()
    end,

    receive
        Any ->
            ?line io:format("Unexpected: ~p", [Any]),
            ?line ?t:fail()
    after 500 ->
            ok
    end,

    ok.


call(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),

    ?line {ok, _Pid} =
        ext_gen_server:start_link({local, my_test_name},
                                  ?MODULE, [], []),

    ?line ok = gen_server:call(my_test_name, started_p),
    ?line delayed = gen_server:call(my_test_name, {delayed_answer,1}),

    %% two requests within a specified time.
    ?line ok = gen_server:call(my_test_name, {call_within, 1000}),
    test_server:sleep(500),
    ?line ok = gen_server:call(my_test_name, next_call),
    ?line ok = gen_server:call(my_test_name, {call_within, 1000}),
    test_server:sleep(1500),
    ?line false = gen_server:call(my_test_name, next_call),

    %% timeout call.
    ?line delayed = gen_server:call(my_test_name, {delayed_answer,1}, 30),
    ?line {'EXIT',{timeout,_}} =
        (catch gen_server:call(my_test_name, {delayed_answer,30}, 1)),

    %% bad return value in the gen_server loop from handle_call.
    ?line {'EXIT',{{bad_return_value, badreturn},_}} =
        (catch gen_server:call(my_test_name, badreturn)),

    process_flag(trap_exit, OldFl),
    ok.


cast(Config) when is_list(Config) ->
    ?line {ok, Pid} =
        ext_gen_server:start({local, my_test_name},
                             ?MODULE, [], []),

    ?line ok = gen_server:call(my_test_name, started_p),

    ?line ok = gen_server:cast(my_test_name, {self(),handle_cast}),
    ?line receive
              {Pid, handled_cast} ->
                  ok
          after 1000 ->
                  test_server:fail(handle_cast)
          end,

    ?line ok = gen_server:cast(my_test_name, {self(),delayed_cast,1}),
    ?line receive
              {Pid, delayed} ->
                  ok
          after 1000 ->
                  test_server:fail(delayed_cast)
          end,

    ?line ok = gen_server:cast(my_test_name, {self(),stop}),
    ?line receive
              {Pid, stopped} ->
                  ok
          after 1000 ->
                  test_server:fail(stop)
          end,
    ok.


info(Config) when is_list(Config) ->
    ?line {ok, Pid} =
        ext_gen_server:start({local, my_test_name},
                             ?MODULE, [], []),

    ?line ok = gen_server:call(my_test_name, started_p),

    ?line Pid ! {self(),handle_info},
    ?line receive
              {Pid, handled_info} ->
                  ok
          after 1000 ->
                  test_server:fail(handle_info)
          end,

    ?line Pid ! {self(),delayed_info,1},
    ?line receive
              {Pid, delayed_info} ->
                  ok
          after 1000 ->
                  test_server:fail(delayed_info)
          end,

    ?line Pid ! {self(),stop},
    ?line receive
              {Pid, stopped_info} ->
                  ok
          after 1000 ->
                  test_server:fail(stop_info)
          end,
    ok.


hibernate(Config) when is_list(Config) ->
    OldFl = process_flag(trap_exit, true),
    {ok, Pid0} =
        ext_gen_server:start_link({local, my_test_name_hibernate0},
                                  ?MODULE, hibernate, []),
    is_in_erlang_hibernate(Pid0),
    ok = gen_server:call(my_test_name_hibernate0, stop),
    receive
        {'EXIT', Pid0, stopped} ->
            ok
    after 5000 ->
            test_server:fail(gen_server_did_not_die)
    end,

    {ok, Pid} =
        ext_gen_server:start_link({local, my_test_name_hibernate},
                                  ?MODULE, [], []),

    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = gen_server:call(my_test_name_hibernate, hibernate),
    is_in_erlang_hibernate(Pid),
    Parent = self(),
    Fun = fun() ->
                  receive go -> ok end,
                  receive after 1000 -> ok end,
                  X = erlang:process_info(Pid, current_function),
                  Pid ! continue,
                  Parent ! {result,X}
          end,
    Pid2 = spawn_link(Fun),
    true = gen_server:call(my_test_name_hibernate, {hibernate_noreply,Pid2}),

    gen_server:cast(my_test_name_hibernate, hibernate_later),
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),

    gen_server:cast(my_test_name_hibernate, hibernate_now),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),

    Pid ! hibernate_later,
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),

    Pid ! hibernate_now,
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/=
                erlang:process_info(Pid, current_function)),
    receive
        {result,R} ->
            {current_function,{erlang,hibernate,3}} = R
    end,

    true = gen_server:call(my_test_name_hibernate, hibernate),
    is_in_erlang_hibernate(Pid),
    sys:suspend(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    sys:resume(my_test_name_hibernate),
    is_in_erlang_hibernate(Pid),
    ok = gen_server:call(my_test_name_hibernate, started_p),
    true = ({current_function,{erlang,hibernate,3}} =/= erlang:process_info(Pid,current_function)),

    ok = gen_server:call(my_test_name_hibernate, stop),
    receive
        {'EXIT', Pid, stopped} ->
            ok
    after 5000 ->
            test_server:fail(gen_server_did_not_die)
    end,
    process_flag(trap_exit, OldFl),
    ok.

is_in_erlang_hibernate(Pid) ->
    receive after 1 -> ok end,
    is_in_erlang_hibernate_1(200, Pid).

is_in_erlang_hibernate_1(0, Pid) ->
    io:format("~p\n", [erlang:process_info(Pid, current_function)]),
    ?t:fail(not_in_erlang_hibernate_3);
is_in_erlang_hibernate_1(N, Pid) ->
    {current_function,MFA} = erlang:process_info(Pid, current_function),
    case MFA of
        {erlang,hibernate,3} ->
            ok;
        _ ->
            receive after 10 -> ok end,
            is_in_erlang_hibernate_1(N-1, Pid)
    end.


busy_wait_for_process(Pid,N) ->
    case erlang:is_process_alive(Pid) of
        true ->
            receive
            after 100 ->
                    ok
            end,
            busy_wait_for_process(Pid,N-1);
        _ ->
            ok
    end.



call_format_status(Config) when is_list(Config) ->
    ?line {ok, Pid} = ext_gen_server:start_link({local, call_format_status},
                                                ?MODULE, [], []),
    ?line Status1 = sys:get_status(call_format_status),
    ?line {status, Pid, _Mod, [_PDict, running, _Parent, _, Data1]} = Status1,
    ?line [format_status_called | _] = lists:reverse(Data1),
    ?line Status2 = sys:get_status(call_format_status, 5000),
    ?line {status, Pid, _Mod, [_PDict, running, _Parent, _, Data2]} = Status2,
    ?line [format_status_called | _] = lists:reverse(Data2),

    %% check that format_status can handle a name being a pid (atom is
    %% already checked by the previous test)
    ?line {ok, Pid3} = ext_gen_server:start_link(?MODULE, [], []),
    ?line Status3 = sys:get_status(Pid3),
    ?line {status, Pid3, _Mod, [_PDict3, running, _Parent, _, Data3]} = Status3,
    ?line [format_status_called | _] = lists:reverse(Data3),
    ok.


error_format_status(Config) when is_list(Config) ->
    ?line error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    State = "called format_status",
    ?line {ok, Pid} = ext_gen_server:start_link(?MODULE, {state, State}, []),
    ?line {'EXIT',{crashed,_}} = (catch gen_server:call(Pid, crash)),
    receive
        {'EXIT', Pid, crashed} ->
            ok
    end,
    receive
        {error,_GroupLeader,{Pid,
                             "** Generic server"++_,
                             [Pid,crash,{formatted, State},
                              {crashed,[{?MODULE,handle_call,3,_}
                                        |_Stacktrace]}]}} ->
            ok;
        Other ->
            ?line io:format("Unexpected: ~p", [Other]),
            ?line ?t:fail()
    end,
    ?t:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.


terminate_crash_format(Config) when is_list(Config) ->
    error_logger_forwarder:register(),
    OldFl = process_flag(trap_exit, true),
    State = crash_terminate,
    {ok, Pid} = ext_gen_server:start_link(?MODULE, {state, State}, []),
    gen_server:call(Pid, stop),
    receive {'EXIT', Pid, {crash, terminate}} -> ok end,
    receive
        {error,_GroupLeader,{Pid,
                             "** Generic server"++_,
                             [Pid,stop,{formatted, State},
                              {{crash, terminate},[{?MODULE,terminate,2,_}
                                                   |_Stacktrace]}]}} ->
            ok;
        Other ->
            io:format("Unexpected: ~p", [Other]),
            ?t:fail()
    after 5000 ->
            io:format("Timeout: expected error logger msg", []),
            ?t:fail()
    end,
    ?t:messages_get(),
    process_flag(trap_exit, OldFl),
    ok.


get_state(Config) when is_list(Config) ->
    State = self(),
    {ok, _Pid} = ext_gen_server:start_link({local, get_state},
                                           ?MODULE, {state,State}, []),
    State = sys:get_state(get_state),
    State = sys:get_state(get_state, 5000),
    {ok, Pid} = ext_gen_server:start_link(?MODULE, {state,State}, []),
    State = sys:get_state(Pid),
    State = sys:get_state(Pid, 5000),
    ok = sys:suspend(Pid),
    State = sys:get_state(Pid),
    ok = sys:resume(Pid),
    ok.


replace_state(Config) when is_list(Config) ->
    State = self(),
    {ok, _Pid} = ext_gen_server:start_link({local, replace_state},
                                           ?MODULE, {state,State}, []),
    State = sys:get_state(replace_state),
    NState1 = "replaced",
    Replace1 = fun(_) -> NState1 end,
    NState1 = sys:replace_state(replace_state, Replace1),
    NState1 = sys:get_state(replace_state),
    {ok, Pid} = ext_gen_server:start_link(?MODULE, {state,NState1}, []),
    NState1 = sys:get_state(Pid),
    Suffix = " again",
    NState2 = NState1 ++ Suffix,
    Replace2 = fun(S) -> S ++ Suffix end,
    NState2 = sys:replace_state(Pid, Replace2, 5000),
    NState2 = sys:get_state(Pid, 5000),
    %% verify no change in state if replace function crashes
    Replace3 = fun(_) -> throw(fail) end,
    {'EXIT',{{callback_failed,
              {ext_gen_server,system_replace_state},{throw,fail}},_}} =
        (catch sys:replace_state(Pid, Replace3)),
    NState2 = sys:get_state(Pid, 5000),
    %% verify state replaced if process sys suspended
    ok = sys:suspend(Pid),
    Suffix2 = " and again",
    NState3 = NState2 ++ Suffix2,
    Replace4 = fun(S) -> S ++ Suffix2 end,
    NState3 = sys:replace_state(Pid, Replace4),
    ok = sys:resume(Pid),
    NState3 = sys:get_state(Pid, 5000),
    ok.



%%% --------------------------------------------------------
%%% Here is the tested gen_server behaviour.
%%% --------------------------------------------------------

init([]) ->
    {ok, []};
init(ignore) ->
    ignore;
init(stop) ->
    {stop, stopped};
init(hibernate) ->
    {ok,[],hibernate};
init(sleep) ->
    test_server:sleep(1000),
    {ok, []};
init({state,State}) ->
    {ok, State}.

handle_call(started_p, _From, State) ->
    io:format("FROZ"),
    {reply,ok,State};
handle_call({delayed_answer, T}, From, _State) ->
    {noreply,{reply_to,From},T};
handle_call({call_within, T}, _From, _) ->
    {reply,ok,call_within,T};
handle_call(next_call, _From, call_within) ->
    {reply,ok,[]};
handle_call(next_call, _From, State) ->
    {reply,false,State};
handle_call(badreturn, _From, _State) ->
    badreturn;
handle_call(hibernate, _From, _State) ->
    {reply,true,[],hibernate};
handle_call({hibernate_noreply,Pid}, From, _State) ->
    Pid ! go,
    {noreply,From,hibernate};
handle_call(stop, _From, State) ->
    {stop,stopped,ok,State};
handle_call(crash, _From, _State) ->
    exit(crashed);
handle_call(exit_shutdown, _From, _State) ->
    exit(shutdown);
handle_call(stop_shutdown, _From, State) ->
    {stop,shutdown,State};
handle_call(shutdown_reason, _From, _State) ->
    exit({shutdown,reason});
handle_call(stop_shutdown_reason, _From, State) ->
    {stop,{shutdown,stop_reason},State}.

handle_cast({From,handle_cast}, State) ->
    From ! {self(), handled_cast},
    {noreply, State};
handle_cast({From,delayed_cast,T}, _State) ->
    {noreply, {delayed_cast,From}, T};
handle_cast(hibernate_now, _State) ->
    {noreply, [], hibernate};
handle_cast(hibernate_later, _State) ->
    timer:send_after(1000,self(),hibernate_now),
    {noreply, []};
handle_cast({From, stop}, State) ->
    io:format("BAZ"),
    {stop, {From,stopped}, State}.

handle_info(timeout, {reply_to, From}) ->
    gen_server:reply(From, delayed),
    {noreply, []};
handle_info(timeout, hibernate_me) -> % Arrive here from
                                                % handle_info(hibernate_later,...)
    {noreply, [], hibernate};
handle_info(hibernate_now, _State) ->  % Arrive here from
                                                % handle_cast({_,hibernate_later},...)
                                                % and by direct ! from testcase
    {noreply, [], hibernate};
handle_info(hibernate_later, _State) ->
    {noreply, hibernate_me, 1000};
handle_info(timeout, call_within) ->
    {noreply, []};
handle_info(timeout, {delayed_cast, From}) ->
    From ! {self(), delayed},
    {noreply, []};
handle_info(timeout, {delayed_info, From}) ->
    From ! {self(), delayed_info},
    {noreply, []};
handle_info({From, handle_info}, _State) ->
    From ! {self(), handled_info},
    {noreply, []};
handle_info({From, delayed_info, T}, _State) ->
    {noreply, {delayed_info, From}, T};
handle_info(continue, From) ->
    gen_server:reply(From,true),
    {noreply, []};
handle_info({From, stop}, State) ->
    {stop, {From,stopped_info}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate({From, stopped}, _State) ->
    io:format("FOOBAR"),
    From ! {self(), stopped},
    ok;
terminate({From, stopped_info}, _State) ->
    From ! {self(), stopped_info},
    ok;
terminate(_, crash_terminate) ->
    exit({crash, terminate});
terminate(_Reason, _State) ->
    ok.

format_status(terminate, [_PDict, State]) ->
    {formatted, State};
format_status(normal, [_PDict, _State]) ->
    format_status_called.
