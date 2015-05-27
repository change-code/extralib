%% This module implement a tap ipv4 backend with UDP.
-module(tap_inet_udp).
-include("gen_tuntap.hrl").
-behaviour(gen_tuntap).

-export([
	open/1,
	close/2]).

%callbacks
-export([
	init/1,
	handle_data/2,
	terminate/2]).

-spec open(Options :: [option()]) ->
	 {ok, Pid :: pid()}
	|{error, Reason :: term()}.
open(Options) ->
	case gen_tuntap:open(?MODULE, Options) of
		{error, Reason} ->
			error_logger:error_msg("[~p] Error: ~p~n", [?MODULE, Reason]),
			{error, Reason};
		{ok, State} ->
			{ok, spawn_link(gen_tuntap, enter_loop,[?MODULE, State])}
	end.

-spec close(
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #tuntap{}) ->
	term().
close(normal, State) ->
	gen_tuntap:close(?MODULE, normal, State).

%callbacks
-spec init(State :: #tuntap{}) ->
	 {ok, NewState :: #tuntap{}}
	|{error, Reason :: term()}.
init(#tuntap{port = Port, mode = Mode, ifname = Ifname, state = InternalState} = State) ->
	case Mode of
		tun ->
			{error, "Uncompatible mode."};
		tap ->
			%% TODO: setup InternalState
			{ok, State}
	end.

-spec handle_data(Data :: binary(), IState :: term()) ->
	 {reply, Reply :: binary(), NewIState :: term()}
	|{noreply, NewIState :: term()}
	|{stop, Reason :: term(), NewIState :: term()}
	|{stop, Reason :: term(), Reply :: binary(), NewIState :: term()}.
handle_data(Data, IState) ->
	%%TODO:
	%%Demo: echo back
	{reply, Data, IState}.

-spec terminate(
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #tuntap{}) ->
	term().
terminate(normal, #tuntap{port = Port, mode = Mode, ifname = Ifname, state = InternalState} = State) ->
	%% TODO: clean up InternalState
	{ok, State}.

