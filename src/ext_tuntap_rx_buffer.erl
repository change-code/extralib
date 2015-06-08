-module(ext_tuntap_rx_buffer).

-behaviour(ext_gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, system_code_change/4]).


start_link(Name, Options) ->
    start_link(self(), Name, Options).

start_link(Parent, Name, Options) ->
    ext_gen_server:start_link(?MODULE, [Parent, Name, Options], []).


init([Parent, Name, Options]) ->
    {ok, State} = ext_packet_buffer:init([Parent, [{active, 1}|Options]]),
    {ok, Port} = ext_tuntap_port:start(Name, Options),
    {ok, State#{port => Port}}.


handle_call(get_port, _From, State = #{port:=Port}) ->
    {reply, Port, State};
handle_call(Request, From, State) ->
    ext_packet_buffer:handle_call(Request, From, State).


handle_cast(Msg, State) ->
    ext_packet_buffer:handle_cast(Msg, State).


handle_info(Info, State) ->
    ext_packet_buffer:handle_info(Info, State).


terminate(Reason, State) ->
    ext_packet_buffer:terminate(Reason, State).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

system_code_change(State, ?MODULE, OldVsn, Extra) ->
    case catch ?MODULE:code_change(OldVsn, State, Extra) of
        {ok, NewState} -> {ok, NewState};
        Else -> Else
    end;
system_code_change(State, ext_packet_buffer, OldVsn, Extra) ->
    case catch ext_packet_buffer:code_change(OldVsn, State, Extra) of
        {ok, NewState} -> {ok, NewState};
        Else -> Else
    end;
system_code_change(State, _, _OldVsn, _Extra) ->
    {ok, State}.
