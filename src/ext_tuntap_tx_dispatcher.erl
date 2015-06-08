-module(ext_tuntap_tx_dispatcher).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(RX) ->
    start_link(self(), RX).

start_link(Parent, RX) ->
    gen_server:start_link(?MODULE, [Parent, RX], []).


init([Owner, RX]) ->
    Port = gen_server:call(RX, get_port),
    {ok, Buffer} = ext_packet_buffer:start_link([{active, 1}]),
    {ok,
     #{port   => Port,
       buffer => Buffer,
       owner  => Owner}}.


handle_call(get_buffer, {Owner, _}, State = #{owner:=Owner, buffer:=Buffer}) ->
    {reply, Buffer, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({_, {data, Packet}}, State = #{buffer:=Buffer, port:=Port}) ->
    ext_tuntap_port:send(Port, Packet),
    gen_server:call(Buffer, {setopts, [{active, 1}]}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
