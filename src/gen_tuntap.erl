-module(gen_tuntap).
-include("gen_tuntap.hrl").
-export([
	open/2,
	enter_loop/2,
	close/3]).

-callback init(State :: #tuntap{}) ->
	 {ok, NewState :: #tuntap{}}
	|{error, Reason :: term()}.

%% handle_data talks to the Port(tun/tap device)
-callback handle_data(Data :: binary(), IState :: term()) ->
	 {reply, Reply :: binary(), NewIState :: term()}
	|{noreply, NewIState :: term()}
	|{stop, Reason :: term(), NewIState :: term()}
	|{stop, Reason :: term(), Reply :: binary(), NewIState :: term()}.

-callback terminate(
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #tuntap{}) ->
	term().

-spec open(Module :: atom(),
	Options :: [option()]) ->
	 {ok, State :: #tuntap{}}
	|{error, Reason :: term()}.
open(Module, Options) ->
	Mode = proplists:get_value(mode, Options),
	Args_mode = case Mode of
		tun ->
			<<"tun">>;
		tap ->
			<<"tap">>;
		_ ->
			undefined
	end,

	Ifname = proplists:get_value(ifname, Options),
	Args_ifname = case Ifname of
		undefined ->
			<<>>;
		_ ->
			list_to_binary(Ifname)
	end,

	Args = [<<"--mode=", Args_mode/binary>>, Args_ifname],
	Dir = code:lib_dir(extralib, priv),
	Port = erlang:open_port(
		{spawn_executable, [Dir,"/ext_tuntap_port"]},
		[{packet, 2},
		 binary,
		 {args, Args}]),
	process_flag(trap_exit, true),
	link(Port),

	{ok, State} = receive
		{Port, {data, <<"ok.",Real_ifname/binary>>}} ->
			{ok, #tuntap{port = Port, mode = Mode, ifname = binary_to_list(Real_ifname)}};
		{Port, {data, Data}} ->
			{error, Data}
	end,
	init(Module, State).
-spec loop(Module :: atom(), State :: #tuntap{}) -> loop.
loop(Module, State) ->
	#tuntap{port = Port, state = IState} = State,
	receive
		{'EXIT', Port, Reason} ->
			close(Module, {shutdown, "Port exit:" ++ Reason}, State);
		{_Pid, {send, Data}} ->
			port_command(Port, Data);
		{Port, {data, Data}} ->
			case Module:handle_data(Data, IState) of
				{reply, Reply, NewIState} ->
					port_command(Port, Reply),
					NewState = State#tuntap{state = NewIState},
					loop(Module, NewState);
				{noreply, NewIState} ->
					NewState = State#tuntap{state = NewIState},
					loop(Module, NewState);
				{stop, Reason, NewIState} ->
					error_logger:info_msg("~p: ~p~nclosing Port", [?MODULE, Reason]),
					NewState = State#tuntap{state = NewIState},
					close(Module, normal, NewState);
				{stop, Reason, Reply, NewIState} ->
					port_command(Port, Reply),
					error_logger:info_msg("~p: ~p~nclosing Port", [?MODULE, Reason]),
					NewState = State#tuntap{state = NewIState},
					close(Module, normal, NewState)
			end
	end.

-spec enter_loop(Module :: atom(), State :: #tuntap{}) -> loop.
enter_loop(Module, State) ->
	loop(Module, State).

-spec close(
	Module :: atom(),
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #tuntap{}) ->
	term().

close(Module, normal, State) ->
	{ok, NewState} = terminate(Module, normal , State),
	#tuntap{port = Port} = NewState,
	port_close(Port);
close(Module, {shutdown, Reason}, State) ->
	{ok, NewState} = terminate(Module, {shutdown, Reason} , State),
	#tuntap{port = Port} = NewState,
	port_close(Port).

-spec init(Module::atom(), State::#tuntap{}) ->
	 {ok, State :: #tuntap{}}
	|{error, Reason :: term()}.
init(Module, State) ->
	Module:init(State).

-spec terminate(
	Module :: atom(),
	Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State ::#tuntap{})->
	term().
terminate(Module, normal, State) ->
	Module:terminate(normal, State);
terminate(Module, {shutdown, Reason}, State) ->
	Module:terminate({shutdown, Reason}, State).
